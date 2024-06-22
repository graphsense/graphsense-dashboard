module Update.Pathfinder.Network exposing (addAddress, addTx, animateAddresses, animateTxs, updateAddress, updateTx)

import Animation as A exposing (Animation)
import Api.Data
import Basics.Extra exposing (uncurry)
import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Dict
import Dict.Nonempty as NDict
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Tx as Tx
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
import RecordSetter exposing (s_incomingTxs, s_outgoingTxs, s_visible)
import RemoteData exposing (RemoteData(..))
import Set
import Tuple exposing (first, pair)
import Update.Pathfinder.Tx as Tx


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


toAddresses : Network -> List Id -> List Address
toAddresses model io =
    io
        |> List.filterMap (\a -> Dict.get a model.addresses)


findAddressCoordsNextToTx : Network -> Direction -> Tx -> Maybe Coords
findAddressCoordsNextToTx model direction tx =
    let
        siblings =
            case ( direction, tx.type_ ) of
                ( Outgoing, Tx.Utxo t ) ->
                    Just
                        ( t.outputs
                            |> NDict.toList
                            |> List.map first
                        , t.x
                        , A.getTo t.y
                        )

                ( Incoming, Tx.Utxo t ) ->
                    Just
                        ( t.inputs
                            |> NDict.toList
                            |> List.map first
                        , t.x
                        , A.getTo t.y
                        )

                ( Outgoing, Tx.Account t ) ->
                    Dict.get t.from model.addresses
                        |> Maybe.map
                            (\a ->
                                ( [ t.from ]
                                , a.x
                                , A.getTo a.y
                                )
                            )

                ( Incoming, Tx.Account t ) ->
                    Dict.get t.from model.addresses
                        |> Maybe.map
                            (\a ->
                                ( [ t.to ]
                                , a.x
                                , A.getTo a.y
                                )
                            )
    in
    siblings
        |> Maybe.map
            (\( sibs, x, y ) ->
                { x = x + Direction.signOffsetByDirection direction nodeXOffset
                , y =
                    sibs
                        |> toAddresses model
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
                    (\a -> A.getTo a.y < coords.y)

        diff y =
            let
                d =
                    abs (coords.y - y)
                        - nodeYOffset
            in
            if d < 0 then
                Just d

            else
                Nothing

        add d addr =
            let
                y =
                    A.getTo addr.y
            in
            --if A.isDone addr.clock addr.y then
            { addr
                | y =
                    A.animation 0
                        |> A.from y
                        |> A.to (y + d)
                        |> A.duration 500
                , clock = 0
            }

        {- else
           { addr
               | y =
                   A.retarget addr.clock (y + d) addr.y
        -}
        moveThings s subset =
            Maybe.andThen diff
                >> Maybe.map
                    (\d -> List.map (add <| d * s) subset)
                >> Maybe.withDefault []

        movedAddresses =
            (getMaxY above
                |> moveThings 1 above
            )
                ++ (getMinY below
                        |> moveThings -1 below
                   )

        newAddresses =
            movedAddresses
                |> List.foldl (\a -> Dict.insert a.id a) model.addresses

        ( txsAbove, txsBelow ) =
            model.txs
                |> Dict.values
                |> List.filterMap
                    (\tx ->
                        case tx.type_ of
                            Tx.Utxo t ->
                                if t.x > coords.x - 1 && t.x < coords.x + 1 then
                                    Just { id = tx.id, x = t.x, y = t.y, clock = t.clock }

                                else
                                    Nothing

                            Tx.Account _ ->
                                Nothing
                    )
                |> List.partition
                    (\a -> A.getTo a.y <= coords.y)

        movedTxs =
            (getMaxY txsAbove |> moveThings 1 txsAbove)
                ++ (getMinY txsBelow |> moveThings -1 txsBelow)

        newTxs =
            movedTxs
                |> List.foldl
                    (\t ->
                        Dict.update t.id
                            (Maybe.map
                                (Tx.updateUtxo
                                    (\utxo ->
                                        { utxo
                                            | y = t.y
                                            , clock = t.clock
                                        }
                                    )
                                )
                            )
                    )
                    model.txs
    in
    { model
        | addresses = newAddresses
        , txs = newTxs
        , animatedAddresses =
            movedAddresses
                |> List.map .id
                |> Set.fromList
                |> Set.union model.animatedAddresses
        , animatedTxs =
            movedTxs
                |> List.map .id
                |> Set.fromList
                |> Set.union model.animatedTxs
    }


placeAddress : Coords -> Id -> Network -> Network
placeAddress coords id model =
    let
        ( address, newTxs ) =
            listTxsForAddress model id
                |> List.foldl
                    (\( direction, tx ) ( addr, txs ) ->
                        ( case direction of
                            Incoming ->
                                { addr
                                    | outgoingTxs = Set.insert tx.id addr.outgoingTxs
                                }

                            Outgoing ->
                                { addr
                                    | incomingTxs = Set.insert tx.id addr.incomingTxs
                                }
                        , Dict.update tx.id
                            (Maybe.map
                                (Tx.updateUtxo
                                    (Tx.updateUtxoIo direction addr.id (s_visible True))
                                )
                            )
                            txs
                        )
                    )
                    ( Address.init id coords
                    , model.txs
                    )

        animAddress =
            if hasAnimations model then
                { address | opacity = opacityAnimation }

            else
                address
    in
    { model
        | addresses = Dict.insert id animAddress model.addresses
        , txs = newTxs
        , animatedAddresses = Set.insert id model.animatedAddresses
    }


opacityAnimation : Animation
opacityAnimation =
    A.animation 0
        |> A.from 0
        |> A.to 1
        |> A.duration 500


updateAddress : Id -> (Address -> Address) -> Network -> Network
updateAddress id update model =
    { model | addresses = Dict.update id (Maybe.map update) model.addresses }


updateTx : Id -> (Tx -> Tx) -> Network -> Network
updateTx id update model =
    { model | txs = Dict.update id (Maybe.map update) model.txs }


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


getMaxY : List { a | y : Animation } -> Maybe Float
getMaxY =
    List.map (.y >> A.getTo)
        >> List.maximum


getMinY : List { a | y : Animation } -> Maybe Float
getMinY =
    List.map (.y >> A.getTo)
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


addTx : Api.Data.Tx -> Network -> Network
addTx tx network =
    let
        id =
            case tx of
                Api.Data.TxTxAccount t ->
                    Id.init t.currency t.txHash

                Api.Data.TxTxUtxo t ->
                    Id.init t.currency t.txHash
    in
    if Dict.member id network.txs then
        network

    else
        case tx of
            Api.Data.TxTxAccount t ->
                Tx.fromTxAccountData t
                    |> insertTx network

            Api.Data.TxTxUtxo t ->
                let
                    coords =
                        findUtxoTxCoords t network
                in
                Tx.fromTxUtxoData t coords
                    |> Maybe.map
                        (\tx_ ->
                            let
                                newNetwork =
                                    freeSpaceAroundCoords coords network
                            in
                            Tx.updateUtxo
                                (\utxo ->
                                    if hasAnimations newNetwork then
                                        utxo

                                    else
                                        { utxo
                                            | opacity = opacityAnimation
                                            , clock = 0
                                        }
                                )
                                tx_
                                |> insertTx
                                    { newNetwork
                                        | animatedTxs = Set.insert id newNetwork.animatedTxs
                                    }
                        )
                    |> Maybe.withDefault network


insertTx : Network -> Tx -> Network
insertTx network tx =
    let
        upd dir addr =
            let
                ( get, set ) =
                    case dir of
                        Outgoing ->
                            ( .incomingTxs, s_incomingTxs )

                        Incoming ->
                            ( .outgoingTxs, s_outgoingTxs )
            in
            if Set.member tx.id <| get addr then
                addr

            else
                set (Set.insert tx.id <| get addr) addr

        updTx dir a =
            Tx.updateUtxo
                (Tx.updateUtxoIo dir a.id (s_visible True))
    in
    getAddressesForTx network.addresses tx
        |> List.foldl
            (\( dir, a ) nw ->
                { nw
                    | addresses = Dict.update a.id (Maybe.map (upd dir)) nw.addresses
                    , txs = Dict.update tx.id (Maybe.map (updTx dir a)) nw.txs
                }
            )
            { network
                | txs = Dict.insert tx.id tx network.txs
            }


findUtxoTxCoords : Api.Data.TxUtxo -> Network -> Coords
findUtxoTxCoords tx network =
    let
        toSet =
            Maybe.withDefault []
                >> List.map .address
                >> List.concat
                -- TODO what to do with multisig? Fine to concat?
                >> Set.fromList

        normalizeAddresses direction =
            Set.toList
                >> List.filterMap
                    (\a ->
                        Dict.get (Id.init tx.currency a) network.addresses
                    )
                >> List.map (pair direction)

        inputSet =
            tx.inputs
                |> toSet

        outputSet =
            tx.outputs
                |> toSet
                |> Set.filter
                    (\o -> Set.member o inputSet |> not)
    in
    normalizeAddresses Outgoing inputSet
        ++ normalizeAddresses Incoming outputSet
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
                                Coords address.x <| A.getTo address.y
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
                                |> Maybe.map
                                    (\tx ->
                                        (case direction of
                                            Outgoing ->
                                                tx.outputs

                                            Incoming ->
                                                tx.inputs
                                        )
                                            |> NDict.toList
                                            |> List.map first
                                            |> toAddresses model
                                    )
                    )
                |> List.concat

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
            |> Maybe.withDefault (A.getTo address.y)
    }


animateAddresses : Float -> Network -> Network
animateAddresses delta model =
    model.animatedAddresses
        |> Set.foldl
            (\id network ->
                Dict.get id network.addresses
                    |> Maybe.map
                        (\addr ->
                            let
                                clock =
                                    addr.clock + delta
                            in
                            { network
                                | addresses =
                                    Dict.insert id
                                        { addr
                                            | clock = clock
                                            , opacity =
                                                if A.isDone clock addr.opacity then
                                                    A.static 1

                                                else
                                                    addr.opacity
                                        }
                                        network.addresses
                                , animatedAddresses =
                                    if A.isDone clock addr.y && A.isDone clock addr.opacity then
                                        Set.remove id network.animatedAddresses

                                    else
                                        network.animatedAddresses
                            }
                        )
                    |> Maybe.withDefault network
            )
            model


animateTxs : Float -> Network -> Network
animateTxs delta model =
    model.animatedTxs
        |> Set.foldl
            (\id network ->
                Dict.get id network.txs
                    |> Maybe.map
                        (\tx ->
                            case tx.type_ of
                                Tx.Account _ ->
                                    network

                                Tx.Utxo t ->
                                    let
                                        clock =
                                            t.clock + delta
                                    in
                                    { network
                                        | txs =
                                            Dict.insert id
                                                { tx
                                                    | type_ =
                                                        { t
                                                            | clock = clock
                                                            , opacity =
                                                                if A.isDone clock t.opacity then
                                                                    A.static 1

                                                                else
                                                                    t.opacity
                                                        }
                                                            |> Tx.Utxo
                                                }
                                                network.txs
                                        , animatedTxs =
                                            if A.isDone clock t.y && A.isDone clock t.opacity then
                                                Set.remove id network.animatedTxs

                                            else
                                                network.animatedTxs
                                    }
                        )
                    |> Maybe.withDefault network
            )
            model
