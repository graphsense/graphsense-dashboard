module Update.Pathfinder.Network exposing (addAddress, addTx, updateAddress)

import Api.Data
import Basics.Extra exposing (uncurry)
import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Dict
import Dict.Nonempty as NDict
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import List.Nonempty as NList
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Id as Id
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Id.Address as Address
import Model.Pathfinder.Id.Tx as Tx
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Tx as Tx exposing (Tx, getAddressesForTx)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter exposing (s_incomingTxs, s_outgoingTxs)
import RemoteData exposing (RemoteData(..))
import Set
import Tuple exposing (first, pair)


addAddress : Id -> Network -> Network
addAddress id model =
    if Dict.member id model.addresses then
        model

    else
        let
            coords =
                findAddressCoords id model
        in
        freeSpaceAroundCoords coords model
            |> placeAddress coords id


findAddressCoordsNextToTx : Network -> Direction -> Tx -> Maybe Coords
findAddressCoordsNextToTx model direction tx =
    let
        toAddresses io =
            io
                |> NDict.toList
                |> List.filterMap (first >> (\a -> Dict.get a model.addresses))

        siblings =
            case ( direction, tx.type_ ) of
                ( Outgoing, Tx.Utxo t ) ->
                    Just ( t.outputs, t.x, t.y )

                ( Incoming, Tx.Utxo t ) ->
                    Just ( t.inputs, t.x, t.y )

                ( Outgoing, Tx.Account t ) ->
                    Dict.get t.from model.addresses
                        |> Maybe.map
                            (\a ->
                                ( NDict.singleton t.from t.value
                                , a.x
                                , a.y
                                )
                            )

                ( Incoming, Tx.Account t ) ->
                    Dict.get t.from model.addresses
                        |> Maybe.map
                            (\a ->
                                ( NDict.singleton t.to t.value
                                , a.x
                                , a.y
                                )
                            )
    in
    siblings
        |> Maybe.map
            (\( sibs, x, y ) ->
                { x = x + Direction.signOffsetByDirection direction nodeXOffset
                , y =
                    sibs
                        |> toAddresses
                        |> getMaxY
                        |> Maybe.map ((+) nodeYOffset)
                        |> Maybe.withDefault y
                }
            )


freeSpaceAroundCoords : Coords -> Network -> Network
freeSpaceAroundCoords coords model =
    let
        ( above, below ) =
            model.addresses
                |> Dict.values
                |> List.filter
                    (\a -> a.x > coords.x - 1 && a.x < coords.x + 1)
                |> List.partition
                    (\a -> a.y < coords.y)

        diff y =
            coords.y - y

        add d addr =
            { addr | y = addr.y + (d - nodeYOffset) }

        moveThings subset =
            Maybe.map diff
                >> Maybe.map
                    (\d -> List.map (add d) subset)
                >> Maybe.withDefault subset

        newAddresses =
            (getMaxY above
                |> moveThings above
            )
                ++ (getMinY below
                        |> moveThings below
                   )
                |> List.foldl (\a -> Dict.insert a.id a) model.addresses

        ( txsAbove, txsBelow ) =
            model.txs
                |> Dict.values
                |> List.filterMap
                    (\tx ->
                        case tx.type_ of
                            Tx.Utxo t ->
                                if t.x > coords.x - 1 && t.x < coords.x + 1 then
                                    Just { id = tx.id, x = t.x, y = t.y }

                                else
                                    Nothing

                            Tx.Account _ ->
                                Nothing
                    )
                |> List.partition
                    (\a -> a.y <= coords.y)

        newTxs =
            (getMaxY txsAbove |> moveThings txsAbove)
                ++ (getMinY txsBelow |> moveThings txsBelow)
                |> List.foldl
                    (\t ->
                        Dict.update t.id
                            (Maybe.map
                                (\tx ->
                                    case tx.type_ of
                                        Tx.Utxo utxo ->
                                            { tx | type_ = Tx.Utxo { utxo | y = t.y } }

                                        Tx.Account _ ->
                                            tx
                                )
                            )
                    )
                    model.txs
    in
    { model
        | addresses = newAddresses
        , txs = newTxs
    }


placeAddress : Coords -> Id -> Network -> Network
placeAddress coords id model =
    let
        address =
            listTxsForAddress model id
                |> List.foldl
                    (\( direction, tx ) addr ->
                        case direction of
                            Incoming ->
                                { addr
                                    | outgoingTxs = Set.insert tx.id addr.outgoingTxs
                                }

                            Outgoing ->
                                { addr
                                    | incomingTxs = Set.insert tx.id addr.incomingTxs
                                }
                    )
                    (Address.init id coords)
    in
    { model | addresses = Dict.insert id address model.addresses }


updateAddress : Id -> (Address -> Address) -> Network -> Network
updateAddress id update model =
    { model | addresses = Dict.update id (Maybe.map update) model.addresses }


findFreeCoords : Network -> Coords
findFreeCoords model =
    { x = 0
    , y =
        model.addresses
            |> Dict.values
            |> getMinY
            |> Maybe.map ((+) nodeYOffset)
            |> Maybe.withDefault
                (model.txs
                    |> Dict.values
                    |> List.filterMap Tx.getUtxoTx
                    |> getMinY
                    |> Maybe.withDefault 0
                )
    }


getMaxY : List { a | y : Float } -> Maybe Float
getMaxY =
    List.map .y
        >> List.maximum


getMinY : List { a | y : Float } -> Maybe Float
getMinY =
    List.map .y
        >> List.minimum


findAddressCoords : Id -> Network -> Coords
findAddressCoords id network =
    listTxsForAddress network id
        |> NList.fromList
        |> Maybe.andThen
            (\list ->
                if NList.length list == 1 then
                    NList.head list
                        |> uncurry (findAddressCoordsNextToTx network)

                else
                    list
                        |> NList.foldl
                            (\( _, tx ) lst ->
                                Tx.getCoords tx
                                    |> Maybe.map (\t -> t :: lst)
                                    |> Maybe.withDefault lst
                            )
                            []
                        |> NList.fromList
                        |> Maybe.map Coords.avg
            )
        |> Maybe.withDefault (findFreeCoords network)


addTx : Id -> Api.Data.Tx -> Network -> Network
addTx id tx network =
    if Dict.member id network.txs then
        network

    else
        case tx of
            Api.Data.TxTxAccount t ->
                fromTxAccountData t
                    |> insertTx network

            Api.Data.TxTxUtxo t ->
                let
                    coords =
                        findUtxoTxCoords t network
                            |> Debug.log "coords"
                in
                fromTxUtxoData t coords
                    |> Maybe.map
                        (insertTx (freeSpaceAroundCoords coords network))
                    |> Maybe.withDefault network


insertTx : Network -> Tx -> Network
insertTx network tx =
    let
        upd addr =
            let
                ( get, set ) =
                    if Tx.hasOutput addr.id tx then
                        ( .incomingTxs, s_incomingTxs )

                    else
                        ( .outgoingTxs, s_outgoingTxs )
            in
            if Set.member tx.id <| get addr then
                addr

            else
                set (Set.insert tx.id <| get addr) addr
    in
    getAddressesForTx network.addresses tx
        |> List.foldl
            (\a nw ->
                { nw
                    | addresses =
                        Dict.update a.id
                            (Maybe.map
                                upd
                            )
                            nw.addresses
                }
            )
            { network
                | txs = Dict.insert tx.id tx network.txs
            }


fromTxAccountData : Api.Data.TxAccount -> Tx
fromTxAccountData tx =
    let
        id =
            Id.init tx.currency tx.txHash
    in
    { id = id
    , type_ =
        Tx.Account
            { from = Id.init tx.currency tx.fromAddress
            , to = Id.init tx.currency tx.toAddress
            , value = tx.value
            }
    , raw = Api.Data.TxTxAccount tx
    }


fromTxUtxoData : Api.Data.TxUtxo -> Coords -> Maybe Tx
fromTxUtxoData tx coords =
    let
        id =
            Id.init tx.currency tx.txHash

        fn dir =
            let
                field =
                    case dir of
                        Incoming ->
                            .inputs

                        Outgoing ->
                            .outputs

                toPair : Api.Data.TxValue -> Maybe ( Id, Api.Data.Values )
                toPair { address, value } =
                    -- TODO what to do with multisig?
                    List.head address
                        |> Maybe.map (\a -> ( Id.init tx.currency a, value ))
            in
            field tx
                |> Maybe.map (List.filterMap toPair)
                |> Maybe.andThen NList.fromList
                |> Maybe.map NDict.fromNonemptyList
    in
    Maybe.map2
        (\in_ out ->
            { id = id
            , type_ =
                Tx.Utxo
                    { x = coords.x
                    , y = coords.y
                    , inputs = in_
                    , outputs = out
                    }
            , raw = Api.Data.TxTxUtxo tx
            }
        )
        (fn Incoming)
        (fn Outgoing)


findUtxoTxCoords : Api.Data.TxUtxo -> Network -> Coords
findUtxoTxCoords tx network =
    let
        normalizeAddresses direction =
            Maybe.withDefault []
                >> List.map .address
                >> List.concat
                -- TODO what to do with multisig? Fine to concat?
                >> List.filterMap
                    (\a ->
                        Dict.get (Id.init tx.currency a) network.addresses
                    )
                >> List.map (pair direction)
    in
    (tx.inputs |> normalizeAddresses Outgoing)
        ++ (tx.outputs |> normalizeAddresses Incoming)
        |> NList.fromList
        |> Maybe.map
            (\list ->
                if NList.length list == 1 then
                    NList.head list
                        |> uncurry (findUtxoTxCoordsNextToAddress network)

                else
                    list
                        |> NList.map
                            (\( _, address ) ->
                                Coords address.x address.y
                            )
                        |> Coords.avg
            )
        |> Maybe.withDefault (findFreeCoords network)


findUtxoTxCoordsNextToAddress : Network -> Direction -> Address -> Coords
findUtxoTxCoordsNextToAddress model direction address =
    let
        toSiblings io =
            io
                |> Set.toList
                |> List.filterMap
                    (\a ->
                        if a == address.id then
                            Nothing

                        else
                            Dict.get a model.txs
                                |> Maybe.andThen Tx.getUtxoTx
                    )

        siblings =
            case direction of
                Outgoing ->
                    toSiblings address.outgoingTxs

                Incoming ->
                    toSiblings address.incomingTxs
    in
    { x = address.x + Direction.signOffsetByDirection direction nodeXOffset
    , y =
        getMaxY siblings
            |> Maybe.map ((+) nodeYOffset)
            |> Maybe.withDefault address.y
    }