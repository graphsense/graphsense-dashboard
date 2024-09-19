module Update.Pathfinder.Network exposing
    ( FindPosition(..)
    , addAddress
    , addAddressWithPosition
    , addTx
    , addTxWithPosition
    , animateAddresses
    , animateTxs
    , clearSelection
    , deleteAddress
    , deleteTx
    , ingestAddresses
    , ingestTxs
    , snapToGrid
    , updateAddress
    , updateTx
    )

import Animation as A exposing (Animation)
import Api.Data
import Basics.Extra exposing (uncurry)
import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Dict
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Tx as Tx
import List.Nonempty as NList
import Maybe.Extra
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Id as Id
import Model.Pathfinder.Address exposing (Address, txsToSet)
import Model.Pathfinder.Deserialize exposing (DeserializedThing)
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Id.Address as Address
import Model.Pathfinder.Id.Tx as Tx
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Tx as Tx exposing (Tx, listAddressesForTx)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Set
import Svg.Attributes exposing (y)
import Tuple exposing (first, pair, second)
import Tuple2 exposing (pairTo)
import Update.Pathfinder.Address as Address exposing (txsInsertId)
import Update.Pathfinder.Tx as Tx


clearSelection : Network -> Network
clearSelection n =
    { n
        | addresses = Dict.map (\_ v -> v |> s_selected False) n.addresses
        , txs = Dict.map (\_ v -> v |> s_selected False) n.txs
    }


nearestMultiple : Float -> Float -> Float
nearestMultiple n multi =
    (round (n / multi) |> toFloat) * multi


coordsToInt : { t | x : Float, y : A.Animation, dx : Float, dy : Float } -> { t | x : Float, y : A.Animation, dx : Float, dy : Float }
coordsToInt item =
    { item | x = nearestMultiple (item.x + item.dx) nodeXOffset, y = A.static (nearestMultiple (A.getTo item.y + item.dy) nodeYOffset), dx = 0, dy = 0 }


snapToGrid : Network -> Network
snapToGrid n =
    let
        mn =
            { n | txs = Dict.map (\_ v -> coordsToInt v) n.txs, addresses = Dict.map (\_ v -> coordsToInt v) n.addresses }

        -- for all Utxos reset addresses.
        updateIoAddresses utxo =
            let
                u a =
                    Dict.get a.id mn.addresses |> Maybe.withDefault a
            in
            { utxo | inputs = utxo.inputs |> Dict.map (\_ -> Tx.updateAddress u), outputs = utxo.outputs |> Dict.map (\_ -> Tx.updateAddress u) }

        updateIosAddresses _ =
            Tx.updateUtxo updateIoAddresses
    in
    mn |> s_txs (mn.txs |> Dict.map updateIosAddresses)


addAddress : Id -> Network -> Network
addAddress =
    addAddressWithPosition Auto


addAddressWithPosition : FindPosition -> Id -> Network -> Network
addAddressWithPosition position id model =
    if Dict.member id model.addresses then
        model

    else
        let
            things =
                listTxsForAddress model id
                    |> List.map Tuple.second

            coords =
                avoidOverlappingEdges things <|
                    case position of
                        Auto ->
                            findAddressCoords id model

                        NextTo ( direction, id_ ) ->
                            Dict.get id_ model.txs
                                |> Maybe.andThen
                                    (findAddressCoordsNextToTx model direction)
                                |> Maybe.Extra.withDefaultLazy
                                    (\_ ->
                                        findAddressCoords id model
                                    )
        in
        Address.init id coords
            |> insertAddress (freeSpaceAroundCoords coords model)


avoidOverlappingEdges : List { a | y : Animation } -> Coords -> Coords
avoidOverlappingEdges things coords =
    let
        sameY =
            things
                |> List.filter (\tx -> A.getTo tx.y |> round |> (==) (round coords.y))
                |> List.length
    in
    if sameY > 1 then
        { coords | y = coords.y - toFloat sameY - 1 }

    else
        coords


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
                            |> Dict.toList
                            |> List.map first
                        , tx.x
                        , A.getTo tx.y
                        )

                ( Incoming, Tx.Utxo t ) ->
                    Just
                        ( t.inputs
                            |> Dict.toList
                            |> List.map first
                        , tx.x
                        , A.getTo tx.y
                        )

                ( Outgoing, Tx.Account _ ) ->
                    ( []
                    , tx.x
                    , A.getTo tx.y
                    )
                        |> Just

                ( Incoming, Tx.Account _ ) ->
                    ( []
                    , tx.x
                    , A.getTo tx.y
                    )
                        |> Just
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
                        if tx.x > coords.x - 1 && tx.x < coords.x + 1 then
                            Just { id = tx.id, x = tx.x, y = tx.y, clock = tx.clock }

                        else
                            Nothing
                    )
                |> List.partition
                    (\a -> A.getTo a.y <= coords.y)

        movedTxs =
            (getMaxY txsAbove |> moveThings 1 txsAbove)
                ++ (getMinY txsBelow |> moveThings -1 txsBelow)

        updateUtxo movedTx tx =
            movedAddresses
                |> List.foldl
                    (\a ->
                        Tx.updateUtxo (Tx.updateUtxoIo Incoming a.id (Tx.setAddress a))
                            >> Tx.updateUtxo (Tx.updateUtxoIo Outgoing a.id (Tx.setAddress a))
                    )
                    (tx |> s_y movedTx.y |> s_clock movedTx.clock)

        newTxs =
            movedTxs
                |> List.foldl
                    (\t ->
                        Dict.update t.id
                            (Maybe.map (updateUtxo t))
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


insertAddress : Network -> Address -> Network
insertAddress model newAddress =
    let
        ( address, newTxs ) =
            listTxsForAddress model newAddress.id
                |> List.foldl
                    (\( direction, tx ) ( addr, txs ) ->
                        ( case direction of
                            Incoming ->
                                { addr
                                    | outgoingTxs = txsInsertId tx.id addr.outgoingTxs
                                }

                            Outgoing ->
                                { addr
                                    | incomingTxs = txsInsertId tx.id addr.incomingTxs
                                }
                        , Dict.update tx.id
                            (Maybe.map
                                (Tx.updateUtxo
                                    (Tx.updateUtxoIo direction newAddress.id (Tx.setAddress newAddress))
                                )
                            )
                            txs
                        )
                    )
                    ( newAddress
                    , model.txs
                    )

        animAddress =
            if hasAnimations model then
                { address | opacity = opacityAnimation }

            else
                address
    in
    { model
        | addresses = Dict.insert newAddress.id animAddress model.addresses
        , txs = newTxs
        , animatedAddresses = Set.insert newAddress.id model.animatedAddresses
    }


opacityAnimation : Animation
opacityAnimation =
    A.animation 0
        |> A.from 0
        |> A.to 1
        |> A.duration 500


updateAddress : Id -> (Address -> Address) -> Network -> Network
updateAddress id update model =
    listTxsForAddress model id
        |> List.foldl
            (\( direction, tx ) ->
                updateTx tx.id
                    (Tx.updateUtxo (Tx.updateUtxoIo direction id (Tx.updateAddress update)))
            )
            { model
                | addresses = Dict.update id (Maybe.map update) model.addresses
            }


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
                    -- |> List.filterMap Tx.getUtxoTx
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


type FindPosition
    = Auto
    | NextTo ( Direction, Id )


addTx : Api.Data.Tx -> Network -> ( Tx, Network )
addTx =
    addTxWithPosition Auto


addTxWithPosition : FindPosition -> Api.Data.Tx -> Network -> ( Tx, Network )
addTxWithPosition position tx network =
    let
        id =
            Tx.getTxId tx
    in
    Dict.get id network.txs
        |> Maybe.map (pairTo network)
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                case tx of
                    Api.Data.TxTxAccount t ->
                        let
                            coords =
                                findTxCoords network tx

                            newNetwork =
                                freeSpaceAroundCoords coords network
                        in
                        Tx.fromTxAccountData newNetwork t coords
                            |> s_isStartingPoint (isEmpty network)
                            |> insertTx
                                { newNetwork
                                    | animatedTxs = Set.insert id newNetwork.animatedTxs
                                }

                    Api.Data.TxTxUtxo t ->
                        let
                            things =
                                listInOutputsOfApiTxUtxo network t
                                    |> List.map second

                            coords =
                                avoidOverlappingEdges things <|
                                    case position of
                                        Auto ->
                                            findUtxoTxCoords network t

                                        NextTo ( direction, id_ ) ->
                                            Dict.get id_ network.addresses
                                                |> Maybe.map
                                                    (findUtxoTxCoordsNextToAddress network direction)
                                                |> Maybe.Extra.withDefaultLazy
                                                    (\_ ->
                                                        findUtxoTxCoords network t
                                                    )

                            newNetwork =
                                freeSpaceAroundCoords coords network
                        in
                        Tx.fromTxUtxoData newNetwork t coords
                            |> (\tx_i ->
                                    if hasAnimations newNetwork then
                                        tx_i

                                    else
                                        { tx_i
                                            | opacity = opacityAnimation
                                            , clock = 0
                                        }
                               )
                            |> s_isStartingPoint (isEmpty network)
                            |> insertTx
                                { newNetwork
                                    | animatedTxs = Set.insert id newNetwork.animatedTxs
                                }
            )


insertTx : Network -> Tx -> ( Tx, Network )
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
            if Set.member tx.id <| txsToSet <| get addr then
                addr

            else
                set (get addr |> txsInsertId tx.id) addr

        updTx dir a =
            Tx.updateUtxo
                (Tx.updateUtxoIo dir a.id (Tx.setAddress a))
    in
    listAddressesForTx network.addresses tx
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
        |> pair tx


listInOutputsOfApiTxUtxo : Network -> Api.Data.TxUtxo -> List ( Direction, Address )
listInOutputsOfApiTxUtxo network tx =
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


findTxCoords : Network -> Api.Data.Tx -> Coords
findTxCoords network tx =
    case tx of
        Api.Data.TxTxAccount t ->
            let
                fromId =
                    Id.init t.network t.fromAddress

                toId =
                    Id.init t.network t.toAddress

                minStep =
                    nodeYOffset / 2

                baseC =
                    findTxCoordsInternal network ([ Dict.get toId network.addresses |> Maybe.map (Tuple.pair Direction.Incoming), Dict.get fromId network.addresses |> Maybe.map (Tuple.pair Direction.Outgoing) ] |> List.filterMap identity)

                allTxs =
                    Dict.values network.txs

                sameParticipantsTxs =
                    allTxs
                        |> List.filter
                            (\x ->
                                case x.type_ of
                                    Tx.Account et ->
                                        et.from == fromId && et.to == toId

                                    _ ->
                                        False
                            )

                candidateTxs =
                    (if List.length sameParticipantsTxs > 0 then
                        sameParticipantsTxs

                     else
                        allTxs
                    )
                        |> List.filter (.x >> (==) baseC.x)

                yn =
                    if List.length candidateTxs > 0 then
                        floor ((candidateTxs |> List.map (.y >> A.getTo) |> List.minimum |> Maybe.withDefault minStep) - minStep) |> toFloat

                    else
                        baseC.y
            in
            { baseC | y = yn } |> avoidOverlappingEdges (Dict.values network.txs)

        Api.Data.TxTxUtxo t ->
            findUtxoTxCoords network t


findTxCoordsInternal : Network -> List ( Direction, Address ) -> Coords
findTxCoordsInternal network coords =
    coords
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


findUtxoTxCoords : Network -> Api.Data.TxUtxo -> Coords
findUtxoTxCoords network tx =
    findTxCoordsInternal network (listInOutputsOfApiTxUtxo network tx)


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
                                            |> Dict.toList
                                            |> List.map first
                                            |> toAddresses model
                                    )
                    )
                |> List.concat

        siblings =
            case direction of
                Outgoing ->
                    txsToSet address.outgoingTxs
                        |> toSiblings

                Incoming ->
                    txsToSet address.incomingTxs
                        |> toSiblings
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

                                newAddr =
                                    { addr
                                        | clock = clock
                                        , opacity =
                                            if A.isDone clock addr.opacity then
                                                A.static 1

                                            else
                                                addr.opacity
                                    }
                            in
                            { network
                                | animatedAddresses =
                                    if A.isDone clock addr.y && A.isDone clock addr.opacity then
                                        Set.remove id network.animatedAddresses

                                    else
                                        network.animatedAddresses
                            }
                                |> updateAddress id (always newAddr)
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
                            let
                                clock =
                                    tx.clock + delta
                            in
                            { network
                                | txs =
                                    Dict.insert id
                                        { tx
                                            | clock = clock
                                            , opacity =
                                                if A.isDone clock tx.opacity then
                                                    A.static 1

                                                else
                                                    tx.opacity
                                        }
                                        network.txs
                                , animatedTxs =
                                    if A.isDone clock tx.y && A.isDone clock tx.opacity then
                                        Set.remove id network.animatedTxs

                                    else
                                        network.animatedTxs
                            }
                        )
                    |> Maybe.withDefault network
            )
            model


deleteAddress : Id -> Network -> Network
deleteAddress id network =
    Dict.get id network.addresses
        |> Maybe.map
            (\_ ->
                listTxsForAddress network id
                    |> List.foldl
                        (\( direction, tx ) ->
                            updateTx tx.id
                                (Tx.updateUtxo (Tx.updateUtxoIo direction id Tx.unsetAddress))
                        )
                        { network
                            | addresses = Dict.remove id network.addresses
                        }
            )
        |> Maybe.withDefault network


deleteTx : Id -> Network -> Network
deleteTx id network =
    Dict.get id network.txs
        |> Maybe.map
            (\tx ->
                { network
                    | addresses =
                        Tx.listAddressesForTx network.addresses tx
                            |> List.foldl
                                (\( _, address ) ->
                                    Dict.insert address.id (Address.removeTx tx.id address)
                                )
                                network.addresses
                    , txs = Dict.remove id network.txs
                }
            )
        |> Maybe.withDefault network


ingestTxs : Network -> List DeserializedThing -> List Api.Data.Tx -> Network
ingestTxs network things txs =
    let
        thingsDict =
            things
                |> List.map (\th -> ( th.id, th ))
                |> Dict.fromList

        toUtxo nw tx th =
            Tx.fromTxUtxoData nw
                tx
                { x = th.x
                , y = th.y
                }
                |> s_isStartingPoint th.isStartingPoint

        toAccount tx th =
            Tx.fromTxAccountData network
                tx
                { x = th.x
                , y = th.y
                }
                |> s_isStartingPoint th.isStartingPoint
    in
    txs
        |> List.foldl
            (\tx nw ->
                (case tx of
                    Api.Data.TxTxUtxo t ->
                        Dict.get (Id.init t.currency t.txHash) thingsDict
                            |> Maybe.map (toUtxo nw t)

                    Api.Data.TxTxAccount t ->
                        Dict.get (Id.init t.currency t.txHash) thingsDict
                            |> Maybe.map (toAccount t)
                )
                    |> Maybe.map (insertTx nw >> second)
                    |> Maybe.withDefault nw
            )
            network


ingestAddresses : Network -> List DeserializedThing -> Network
ingestAddresses network =
    List.foldl
        (\th nw ->
            Address.init th.id
                { x = th.x
                , y = th.y
                }
                |> s_isStartingPoint th.isStartingPoint
                |> insertAddress nw
        )
        network
