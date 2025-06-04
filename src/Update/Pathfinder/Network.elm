module Update.Pathfinder.Network exposing
    ( addAddress
    , addAddressWithPosition
    , addTx
    , addTxWithPosition
    , animateAddresses
    , animateTxs
    , clearSelection
    , deleteAddress
    , deleteDanglingAddresses
    , deleteTx
    , findAddressCoords
    , getYForPathAfterX
    , ingestAddresses
    , ingestTxs
    , snapToGrid
    , updateAddress
    , updateAddressesByClusterId
    , updateTx
    )

import Animation as A exposing (Animation)
import Api.Data
import Basics.Extra exposing (flip, uncurry)
import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Dict
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Tx as Tx
import List.Nonempty as NList
import Maybe.Extra
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address, txsToSet)
import Model.Pathfinder.Deserialize exposing (DeserializedThing)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Tx as Tx exposing (Tx, listAddressesForTx)
import Plugin.Update exposing (Plugins)
import RecordSetter exposing (..)
import Set
import Tuple exposing (pair, second)
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
    { item
        | x = nearestMultiple (item.x + item.dx) nodeXOffset
        , y = A.static (nearestMultiple (A.getTo item.y + item.dy) nodeYOffset)
        , dx = 0
        , dy = 0
    }


snapToGrid : Network -> Network
snapToGrid n =
    n.addresses
        |> Dict.keys
        |> List.foldl (flip updateAddress coordsToInt)
            { n
                | txs = Dict.map (\_ v -> coordsToInt v) n.txs
            }


addAddress : Plugins -> Id -> Network -> ( Address, Network )
addAddress plugins =
    addAddressWithPosition plugins Auto


addAddressWithPosition : Plugins -> FindPosition -> Id -> Network -> ( Address, Network )
addAddressWithPosition plugins position id model =
    Dict.get id model.addresses
        |> Maybe.map (pairTo model)
        |> Maybe.withDefault
            (let
                things =
                    listTxsForAddressByRaw model id
                        |> List.map Tuple.second

                coords =
                    (case position of
                        Auto ->
                            findAddressCoords id model

                        NextTo ( direction, id_ ) ->
                            Dict.get id_ model.txs
                                |> Maybe.map
                                    (findAddressCoordsNextToTx model direction)
                                |> Maybe.Extra.orElseLazy
                                    (\_ ->
                                        Dict.get id_ model.addresses
                                            |> Maybe.map
                                                (findAddressCoordsNextToAddress direction)
                                    )
                                |> Maybe.Extra.orElseLazy
                                    (\_ ->
                                        findAddressCoords id model
                                    )

                        Fixed x y ->
                            Just { x = x, y = y }
                    )
                        |> Maybe.withDefault (findFreeCoords model)
                        |> avoidOverlappingEdges things

                newAddress =
                    Address.init plugins id coords
                        |> s_isStartingPoint (isEmpty model)
             in
             ( newAddress
             , newAddress
                |> insertAddress (freeSpaceAroundCoords coords model)
             )
            )


findAddressCoordsNextToAddress : Direction -> Address -> Coords
findAddressCoordsNextToAddress direction address =
    { x = address.x + Direction.signOffsetByDirection direction (nodeXOffset * 2)
    , y = A.getTo address.y
    }


avoidOverlappingEdges : List { a | x : Float, y : Animation } -> Coords -> Coords
avoidOverlappingEdges things coords =
    let
        sameY =
            things
                -- keep things which are same y-axis as coords
                |> List.filter (\th -> A.getTo th.y |> round |> (==) (round coords.y))
                -- remove things which are direct neighbors of coords
                |> List.filter (\th -> th.x < coords.x - nodeXOffset || th.x > coords.x + nodeXOffset)
                |> List.length
    in
    if sameY > 0 then
        { coords | y = coords.y - nodeYOffset }

    else
        coords


toAddresses : Network -> List Id -> List Address
toAddresses model io =
    io
        |> List.filterMap (\a -> Dict.get a model.addresses)


findAddressCoordsNextToTx : Network -> Direction -> Tx -> Coords
findAddressCoordsNextToTx model direction tx =
    let
        ( sibs, x, y ) =
            case ( direction, tx.type_ ) of
                ( Outgoing, Tx.Utxo t ) ->
                    ( t.outputs
                        |> Dict.keys
                    , tx.x
                    , A.getTo tx.y
                    )

                ( Incoming, Tx.Utxo t ) ->
                    ( t.inputs
                        |> Dict.keys
                    , tx.x
                    , A.getTo tx.y
                    )

                ( Outgoing, Tx.Account _ ) ->
                    ( []
                    , tx.x
                    , A.getTo tx.y
                    )

                ( Incoming, Tx.Account _ ) ->
                    ( []
                    , tx.x
                    , A.getTo tx.y
                    )
    in
    { x = x + Direction.signOffsetByDirection direction nodeXOffset
    , y =
        sibs
            |> toAddresses model
            |> getMaxY
            |> Maybe.map ((+) nodeYOffset)
            |> Maybe.withDefault y
    }


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
                    (\a -> A.getTo a.y < coords.y)

        movedTxs =
            (getMaxY txsAbove |> moveThings 1 txsAbove)
                ++ (getMinY txsBelow |> moveThings -1 txsBelow)
    in
    movedAddresses
        |> List.foldl
            (\a -> updateAddress a.id (always a))
            (movedTxs
                |> List.foldl
                    (\t -> updateTx t.id (s_y t.y >> s_clock t.clock))
                    { model
                        | animatedAddresses =
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
            )


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
                            (Maybe.map (Tx.setAddressInTx direction newAddress))
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
                    (case tx.type_ of
                        Tx.Utxo _ ->
                            Tx.updateUtxo (Tx.updateUtxoIo direction id (Tx.updateAddress update))

                        Tx.Account _ ->
                            Tx.updateAccount (Tx.updateAccountAddress direction id update)
                    )
            )
            { model
                | addresses = Dict.update id (Maybe.map update) model.addresses
            }


updateAddressesByClusterId : Id -> (Address -> Address) -> Network -> Network
updateAddressesByClusterId id update model =
    let
        toUpdate =
            getAddressIdsInCluster
                id
                model

        agg ida n =
            updateAddress ida update n
    in
    toUpdate |> List.foldl agg model


updateTx : Id -> (Tx -> Tx) -> Network -> Network
updateTx id update model =
    { model | txs = Dict.update id (Maybe.map update) model.txs }


getYForPathAfterX : Network -> Float -> Float -> Float
getYForPathAfterX model xBasis yDefault =
    let
        coords item =
            { x = item.x, y = item.y |> A.getTo }

        allCoords =
            ((model.addresses |> Dict.values |> List.map coords)
                ++ (model.txs |> Dict.values |> List.map coords)
            )
                |> List.filter (\c -> c.x > xBasis)
    in
    allCoords |> List.map .y |> List.maximum |> Maybe.map ((+) nodeYOffset) |> Maybe.withDefault yDefault


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


findAddressCoords : Id -> Network -> Maybe Coords
findAddressCoords id network =
    listTxsForAddress network id
        |> NList.fromList
        |> Maybe.andThen
            (\list ->
                if NList.length list == 1 then
                    NList.head list
                        |> uncurry (findAddressCoordsNextToTx network)
                        |> Just

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
                            fromId =
                                Id.init t.currency t.fromAddress

                            toId =
                                Id.init t.currency t.toAddress

                            things =
                                [ Dict.get toId network.addresses
                                , Dict.get fromId network.addresses
                                ]
                                    |> List.filterMap identity

                            coords =
                                case position of
                                    Auto ->
                                        avoidOverlappingEdges things <| findAccountTxCoords network t

                                    NextTo ( direction, id_ ) ->
                                        avoidOverlappingEdges things <|
                                            (Dict.get id_ network.addresses
                                                |> Maybe.map
                                                    (findTxCoordsNextToAddress network direction)
                                                |> Maybe.Extra.withDefaultLazy
                                                    (\_ ->
                                                        findAccountTxCoords network t
                                                    )
                                            )

                                    Fixed x y ->
                                        { x = x, y = y }

                            newNetwork =
                                freeSpaceAroundCoords coords network
                        in
                        Tx.fromTxAccountData t coords
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
                                case position of
                                    Auto ->
                                        avoidOverlappingEdges things <| findUtxoTxCoords network t

                                    NextTo ( direction, id_ ) ->
                                        avoidOverlappingEdges things <|
                                            (Dict.get id_ network.addresses
                                                |> Maybe.map
                                                    (findTxCoordsNextToAddress network direction)
                                                |> Maybe.Extra.withDefaultLazy
                                                    (\_ ->
                                                        findUtxoTxCoords network t
                                                    )
                                            )

                                    Fixed x y ->
                                        { x = x, y = y }

                            newNetwork =
                                freeSpaceAroundCoords coords network
                        in
                        Tx.fromTxUtxoData t coords
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
        newTx =
            -- set from/to address objects
            Tx.getOutputAddressIds tx
                |> List.filterMap (flip Dict.get network.addresses)
                |> List.foldl (Tx.setAddressInTx Outgoing)
                    (Tx.getInputAddressIds tx
                        |> List.filterMap (flip Dict.get network.addresses)
                        |> List.foldl (Tx.setAddressInTx Incoming) tx
                    )

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
    in
    listAddressesForTx newTx
        |> List.foldl
            (\( dir, a ) -> updateAddress a.id (upd dir))
            { network
                | txs = Dict.insert tx.id newTx network.txs
            }
        |> pair newTx


listInOutputsOfApiTxUtxo : Network -> Api.Data.TxUtxo -> List ( Direction, Address )
listInOutputsOfApiTxUtxo network tx =
    let
        toSet =
            Maybe.withDefault []
                >> List.concatMap .address
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


findAccountTxCoords : Network -> Api.Data.TxAccount -> Coords
findAccountTxCoords network tx =
    let
        fromId =
            Id.init tx.network tx.fromAddress

        toId =
            Id.init tx.network tx.toAddress
    in
    [ Dict.get toId network.addresses
        |> Maybe.map (Tuple.pair Direction.Incoming)
    , Dict.get fromId network.addresses
        |> Maybe.map (Tuple.pair Direction.Outgoing)
    ]
        |> List.filterMap identity
        |> findTxCoordsInternal network


findTxCoordsInternal : Network -> List ( Direction, Address ) -> Coords
findTxCoordsInternal network =
    NList.fromList
        >> Maybe.map
            (\list ->
                if NList.length list == 1 then
                    NList.head list
                        |> uncurry (findTxCoordsNextToAddress network)

                else
                    list
                        |> NList.map
                            (\( _, address ) ->
                                Coords address.x <| A.getTo address.y
                            )
                        |> Coords.avg
            )
        >> Maybe.withDefault (findFreeCoords network)


findUtxoTxCoords : Network -> Api.Data.TxUtxo -> Coords
findUtxoTxCoords network tx =
    findTxCoordsInternal network (listInOutputsOfApiTxUtxo network tx)


findTxCoordsNextToAddress : Network -> Direction -> Address -> Coords
findTxCoordsNextToAddress model direction address =
    let
        toSiblings io =
            io
                |> Set.toList
                |> List.filterMap
                    (flip Dict.get model.txs
                        >> Maybe.map
                            (\tx ->
                                (case direction of
                                    Outgoing ->
                                        Tx.getOutputs tx

                                    Incoming ->
                                        Tx.getInputs tx
                                )
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
                                | animatedTxs =
                                    if A.isDone clock tx.y && A.isDone clock tx.opacity then
                                        Set.remove id network.animatedTxs

                                    else
                                        network.animatedTxs
                            }
                                |> updateTx id
                                    (always
                                        { tx
                                            | clock = clock
                                            , opacity =
                                                if A.isDone clock tx.opacity then
                                                    A.static 1

                                                else
                                                    tx.opacity
                                        }
                                    )
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
                                (case tx.type_ of
                                    Tx.Utxo _ ->
                                        Tx.updateUtxo (Tx.updateUtxoIo direction id Tx.unsetAddress)

                                    Tx.Account _ ->
                                        Tx.unsetAccountAddress direction
                                            |> Tx.updateAccount
                                )
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
                Tx.listAddressesForTx tx
                    |> List.foldl
                        (\( _, a ) -> updateAddress a.id (Address.removeTx tx.id))
                        { network
                            | txs = Dict.remove id network.txs
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

        toUtxo tx th =
            Tx.fromTxUtxoData
                tx
                { x = th.x
                , y = th.y
                }
                |> s_isStartingPoint th.isStartingPoint

        toAccount tx th =
            Tx.fromTxAccountData
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
                            |> Maybe.map (toUtxo t)

                    Api.Data.TxTxAccount t ->
                        Dict.get (Id.init t.network t.identifier) thingsDict
                            |> Maybe.map (toAccount t)
                )
                    |> Maybe.map (insertTx nw >> second)
                    |> Maybe.withDefault nw
            )
            network


ingestAddresses : Plugins -> Network -> List DeserializedThing -> Network
ingestAddresses plugins network =
    List.foldl
        (\th nw ->
            Address.init plugins
                th.id
                { x = th.x
                , y = th.y
                }
                |> s_isStartingPoint th.isStartingPoint
                |> insertAddress nw
        )
        network


deleteDanglingAddresses : Network -> List Address -> Network
deleteDanglingAddresses =
    List.foldl
        (\address nw ->
            if Set.isEmpty (txsToSet address.incomingTxs) && Set.isEmpty (txsToSet address.outgoingTxs) then
                deleteAddress address.id nw

            else
                nw
        )
