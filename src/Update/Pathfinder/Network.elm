module Update.Pathfinder.Network exposing
    ( addAddress
    , addAddressWithPosition
    , addConversion
    , addTx
    , addTxWithPosition
    , aggEdgeNeedsData
    , animateAddresses
    , animateTxs
    , clearSelection
    , deleteAddress
    , deleteAggEdge
    , deleteDanglingAddresses
    , deleteTx
    , findAddressCoords
    , getYForPathAfterX
    , ingestAddresses
    , ingestAggEdges
    , ingestTxs
    , insertFetchedEdge
    , rupsertAggEdge
    , snapToGrid
    , updateAddress
    , updateAddressesByClusterId
    , updateAggEdge
    , updateConversionEdge
    , updateTx
    , upsertAggEdgeData
    )

import Animation as A exposing (Animation)
import Api.Data
import Basics.Extra exposing (flip, uncurry)
import Config.Pathfinder as Pathfinder exposing (nodeXOffset, nodeYOffset)
import Dict exposing (Dict)
import Init.Pathfinder.Address as Address
import Init.Pathfinder.AggEdge as AggEdge
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Tx as Tx
import List.Nonempty as NList
import Maybe.Extra
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address, txsToSet)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Deserialize exposing (DeserializedAggEdge, DeserializedThing)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Plugin.Update exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)
import Tuple exposing (first, pair, second)
import Tuple2 exposing (pairTo, swap)
import Update.Pathfinder.Address as Address exposing (txsInsertId)
import Update.Pathfinder.AggEdge as AggEdge
import Update.Pathfinder.ConversionEdge as ConversionEdge
import Update.Pathfinder.Tx as Tx



-- hasConversion : Id -> Network -> Bool
-- hasConversion id network =
--     Dict.member id network.conversions


addConversion : Api.Data.ExternalConversion -> Tx -> Tx -> Network -> Network
addConversion conversion inputTx outputTx network =
    let
        faid =
            inputTx |> Tx.getOutputAddressIds |> List.head

        taid =
            outputTx |> Tx.getInputAddressIds |> List.head

        -- currently we only support conversions between two addresses, and only draw conversion
        -- between the first address of the input and the first address of the output tx
        medge =
            Maybe.map2 pair faid taid
    in
    case medge of
        Just edge ->
            let
                c =
                    ConversionEdge.init conversion edge ( inputTx |> Tx.getAsset, outputTx |> Tx.getAsset )
                        |> s_inputAddress (Dict.get (first edge) network.addresses)
                        |> s_outputAddress (Dict.get (second edge) network.addresses)

                edgeMapUpsertFn =
                    Maybe.map (Set.insert edge) >> Maybe.withDefault (edge |> Set.singleton) >> Just

                edgeUpsertFn =
                    Maybe.map (List.append [ c ]) >> Maybe.withDefault [ c ] >> Just

                conversionsEdgeMap1 =
                    Dict.update (first edge) edgeMapUpsertFn network.conversionsEdgeMap
                        |> Dict.update (second edge) edgeMapUpsertFn
            in
            { network
                | conversions = Dict.update edge edgeUpsertFn network.conversions
                , conversionsEdgeMap = conversionsEdgeMap1
            }

        _ ->
            network


clearSelection : Network -> Network
clearSelection =
    updateAllAddresses (s_selected False)
        >> updateAllTxs (s_selected False)


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
snapToGrid =
    updateAllAddresses coordsToInt
        >> updateAllTxs coordsToInt


addAddress : Plugins -> Pathfinder.Config -> Id -> Network -> ( Address, Network )
addAddress plugins pc =
    addAddressWithPosition plugins pc Auto


addAddressWithPosition : Plugins -> Pathfinder.Config -> FindPosition -> Id -> Network -> ( Address, Network )
addAddressWithPosition plugins pc position id model =
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
                |> insertAddress pc (freeSpaceAroundCoords coords model)
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


insertAddress : Pathfinder.Config -> Network -> Address -> Network
insertAddress pc model newAddress =
    let
        ( address, newNetwork ) =
            listTxsForAddress model newAddress.id
                |> List.foldl
                    (\( direction, tx ) ( addr, nw ) ->
                        ( case direction of
                            Incoming ->
                                { addr
                                    | outgoingTxs = txsInsertId tx.id addr.outgoingTxs
                                }

                            Outgoing ->
                                { addr
                                    | incomingTxs = txsInsertId tx.id addr.incomingTxs
                                }
                        , setAddressInTx pc tx.id direction newAddress nw
                        )
                    )
                    ( newAddress
                    , setAddressInAggEdges newAddress model
                        |> setAddressInConversions newAddress
                    )

        animAddress =
            if hasAnimations model then
                { address | opacity = opacityAnimation }

            else
                address
    in
    { newNetwork
        | addresses = Dict.insert newAddress.id animAddress model.addresses
        , animatedAddresses = Set.insert newAddress.id model.animatedAddresses
    }


setAddressInAggEdges : Address -> Network -> Network
setAddressInAggEdges address network =
    updateAggEdgesById address.id (AggEdge.setAddress (Just address)) network


setAddressInConversions : Address -> Network -> Network
setAddressInConversions address network =
    updateConversionsById address.id (List.map (ConversionEdge.setAddress (Just address))) network


updateAggEdgesById : Id -> (AggEdge -> AggEdge) -> Network -> Network
updateAggEdgesById id update network =
    Dict.get id network.addressAggEdgeMap
        |> Maybe.map
            (Set.foldl
                (\edgeId -> Dict.update edgeId (Maybe.map update))
                network.aggEdges
            )
        |> Maybe.map (flip s_aggEdges network)
        |> Maybe.withDefault network


updateConversionsById : Id -> (List ConversionEdge -> List ConversionEdge) -> Network -> Network
updateConversionsById id update network =
    Dict.get id network.conversionsEdgeMap
        |> Maybe.map
            (Set.foldl
                (\conversionId -> Dict.update conversionId (Maybe.map update))
                network.conversions
            )
        |> Maybe.map (flip s_conversions network)
        |> Maybe.withDefault network


updateAggEdge : ( Id, Id ) -> (AggEdge -> AggEdge) -> Network -> Network
updateAggEdge id upd network =
    { network
        | aggEdges = Dict.update id (Maybe.map upd) network.aggEdges
    }


updateConversionEdge : ( Id, Id ) -> (ConversionEdge -> ConversionEdge) -> Network -> Network
updateConversionEdge id upd network =
    { network
        | conversions = Dict.update id (Maybe.map (List.map upd)) network.conversions
    }


deleteAggEdge : ( Id, Id ) -> Network -> Network
deleteAggEdge aggEdgeId network =
    { network
        | aggEdges = Dict.remove aggEdgeId network.aggEdges
        , fetchedEdges =
            Set.remove aggEdgeId network.fetchedEdges
                |> Set.remove (swap aggEdgeId)
        , addressAggEdgeMap = deleteFromAggEdgeMap aggEdgeId network.addressAggEdgeMap
    }


deleteFromAggEdgeMap : ( Id, Id ) -> Dict Id (Set ( Id, Id )) -> Dict Id (Set ( Id, Id ))
deleteFromAggEdgeMap ( a, b ) map =
    [ a, b ]
        |> List.foldl
            (\id ->
                Dict.update id
                    (Maybe.andThen
                        (\set ->
                            let
                                newSet =
                                    Set.remove ( a, b ) set
                            in
                            if Set.isEmpty newSet then
                                Nothing

                            else
                                Just newSet
                        )
                    )
            )
            map


rupsertAggEdge : Pathfinder.Config -> ( Id, Id ) -> (AggEdge -> AggEdge) -> Network -> Network
rupsertAggEdge _ (( a, b ) as id) upd network =
    let
        aggEdges =
            Maybe.map
                (\edge ->
                    let
                        newEdge =
                            upd edge
                    in
                    if newEdge.a2b == Success Nothing && newEdge.b2a == Success Nothing then
                        -- delete the edge if both relations are empty
                        Nothing

                    else
                        Just newEdge
                )
                >> Maybe.withDefault
                    (AggEdge.init a b
                        |> AggEdge.setAddress (Dict.get a network.addresses)
                        |> AggEdge.setAddress (Dict.get b network.addresses)
                        |> upd
                        |> Just
                    )
                >> Just
                >> Maybe.Extra.join
                |> flip (Dict.update id) network.aggEdges
    in
    { network
        | aggEdges = aggEdges
        , addressAggEdgeMap =
            if Dict.member id aggEdges then
                updateAddressAggEdgeMap id network.addressAggEdgeMap

            else
                deleteFromAggEdgeMap id network.addressAggEdgeMap
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
        |> updateAggEdgesById id (AggEdge.updateAddress id update)
        |> updateConversionsById id (List.map (ConversionEdge.updateAddress id update))


updateAllAddresses : (Address -> Address) -> Network -> Network
updateAllAddresses upd model =
    model.addresses |> Dict.foldl (\id _ -> updateAddress id upd) model


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
    Dict.get id model.txs
        |> Maybe.map
            (\tx ->
                let
                    newTx =
                        update tx
                in
                { model
                    | txs = Dict.insert id newTx model.txs
                }
            )
        |> Maybe.withDefault model


updateAllTxs : (Tx -> Tx) -> Network -> Network
updateAllTxs upd model =
    model.txs |> Dict.foldl (\id _ -> updateTx id upd) model


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


addTx : Pathfinder.Config -> Api.Data.Tx -> Network -> ( Tx, Network )
addTx pc =
    addTxWithPosition pc Auto


addTxWithPosition : Pathfinder.Config -> FindPosition -> Api.Data.Tx -> Network -> ( Tx, Network )
addTxWithPosition pc position tx network =
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
                            |> insertTx pc
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
                            |> insertTx pc
                                { newNetwork
                                    | animatedTxs = Set.insert id newNetwork.animatedTxs
                                }
            )


insertTx : Pathfinder.Config -> Network -> Tx -> ( Tx, Network )
insertTx pc network tx =
    let
        nw =
            Tx.getOutputAddressIds tx
                |> List.filterMap (flip Dict.get network.addresses)
                |> List.foldl (setAddressInTx pc tx.id Outgoing)
                    (Tx.getInputAddressIds tx
                        |> List.filterMap (flip Dict.get network.addresses)
                        |> List.foldl (setAddressInTx pc tx.id Incoming)
                            { network
                                | txs = Dict.insert tx.id tx network.txs
                            }
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
    Dict.get tx.id nw.txs
        |> Maybe.map
            (\newTx ->
                Tx.listAddressesForTx newTx
                    |> List.foldl
                        (\( dir, a ) -> updateAddress a.id (upd dir))
                        nw
                    |> pair newTx
            )
        |> Maybe.withDefault ( tx, network )


setAddressInTx : Pathfinder.Config -> Id -> Direction -> Address -> Network -> Network
setAddressInTx pc txId dir address network =
    let
        nw =
            updateTx txId
                (Tx.setAddressInTx dir address)
                network
    in
    Dict.get txId nw.txs
        |> Maybe.map
            (\newTx -> insertTxInAggEdges pc newTx nw)
        |> Maybe.withDefault nw


makeTxAddressesCrossproduct : ( List Address, List Address ) -> List ( Address, Address )
makeTxAddressesCrossproduct ( inputs, outputs ) =
    List.foldl (\input cp -> List.map (pair input) outputs ++ cp) [] inputs


insertTxInAggEdges : Pathfinder.Config -> Tx -> Network -> Network
insertTxInAggEdges _ tx network =
    Tx.listSeparatedAddressesForTx tx
        |> makeTxAddressesCrossproduct
        |> List.foldl
            (\( input, output ) nw ->
                let
                    edgeId =
                        AggEdge.initId input.id output.id
                in
                { nw
                    | aggEdges =
                        Dict.update edgeId
                            (Maybe.map
                                (\a ->
                                    { a
                                        | txs = Set.insert tx.id a.txs
                                    }
                                )
                                >> Maybe.withDefault
                                    (AggEdge.init input.id output.id
                                        |> AggEdge.setAddress (Just input)
                                        |> AggEdge.setAddress (Just output)
                                        |> s_txs (Set.singleton tx.id)
                                    )
                                >> Just
                            )
                            nw.aggEdges
                    , addressAggEdgeMap =
                        updateAddressAggEdgeMap edgeId nw.addressAggEdgeMap
                            |> updateAddressAggEdgeMap edgeId
                }
            )
            network


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
                    |> (\nw ->
                            Dict.get id nw.addressAggEdgeMap
                                |> Maybe.map
                                    (Set.foldl deleteAggEdge nw)
                                |> Maybe.withDefault nw
                       )
            )
        |> Maybe.withDefault network


deleteTx : Id -> Network -> Network
deleteTx id network =
    Dict.get id network.txs
        |> Maybe.map
            (\tx ->
                let
                    ( inputs, outputs ) =
                        Tx.listSeparatedAddressesForTx tx

                    cp =
                        makeTxAddressesCrossproduct ( inputs, outputs )

                    network2 =
                        inputs
                            ++ outputs
                            |> List.foldl
                                (\a ->
                                    updateAddress a.id (Address.removeTx tx.id)
                                )
                                network
                in
                cp
                    |> List.foldl
                        (\( input, output ) ->
                            let
                                edgeId =
                                    AggEdge.initId input.id output.id
                            in
                            updateAggEdge edgeId
                                (\edge ->
                                    { edge
                                        | txs = Set.remove id edge.txs
                                    }
                                )
                        )
                        { network2
                            | txs = Dict.remove id network2.txs
                        }
            )
        |> Maybe.withDefault network


ingestTxs : Pathfinder.Config -> Network -> List DeserializedThing -> List ( String, Api.Data.Tx ) -> Network
ingestTxs pc network things txs =
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
            (\( requestTxHash, tx ) nw ->
                -- Use the request tx_hash to find the thing with matching ID
                (case tx of
                    Api.Data.TxTxUtxo t ->
                        Dict.get (Id.init t.currency requestTxHash) thingsDict
                            |> Maybe.map (toUtxo t)

                    Api.Data.TxTxAccount t ->
                        Dict.get (Id.init t.network requestTxHash) thingsDict
                            |> Maybe.map (toAccount t)
                )
                    |> Maybe.map (insertTx pc nw >> second)
                    |> Maybe.withDefault nw
            )
            network


ingestAddresses : Plugins -> Pathfinder.Config -> Network -> List DeserializedThing -> Network
ingestAddresses plugins pc network =
    List.foldl
        (\th nw ->
            Address.init plugins
                th.id
                { x = th.x
                , y = th.y
                }
                |> s_isStartingPoint th.isStartingPoint
                |> insertAddress pc nw
        )
        network


ingestAggEdges : Pathfinder.Config -> List DeserializedAggEdge -> Network -> Network
ingestAggEdges pc aggEdges network =
    aggEdges
        |> List.foldl
            (\{ a, b, txs } ->
                s_txs txs
                    |> rupsertAggEdge pc (AggEdge.initId a b)
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


upsertAggEdgeData : Pathfinder.Config -> Id -> Direction -> Api.Data.NeighborAddress -> Network -> Network
upsertAggEdgeData _ id dir neighbor model =
    let
        nid =
            Id.init neighbor.address.currency neighbor.address.address

        aggEdgeId =
            AggEdge.initId id nid

        aggEdge =
            Dict.get aggEdgeId model.aggEdges
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        AggEdge.init id nid
                            |> AggEdge.setAddress (Dict.get id model.addresses)
                            |> AggEdge.setAddress (Dict.get nid model.addresses)
                    )
                |> AggEdge.setRelationData id dir (Success (Just neighbor))
    in
    { model
        | aggEdges =
            Dict.insert aggEdgeId aggEdge model.aggEdges
        , addressAggEdgeMap = updateAddressAggEdgeMap aggEdgeId model.addressAggEdgeMap
    }


updateAddressAggEdgeMap : ( Id, Id ) -> Dict Id (Set ( Id, Id )) -> Dict Id (Set ( Id, Id ))
updateAddressAggEdgeMap id =
    let
        updMap =
            Maybe.map (Set.insert id)
                >> Maybe.withDefault (Set.singleton id)
                >> Just
    in
    Dict.update (first id) updMap
        >> Dict.update (second id) updMap


{-|

    Returns the missing directions of the aggedge

-}
aggEdgeNeedsData : Id -> Id -> Network -> ( Bool, Bool )
aggEdgeNeedsData a b network =
    ( Set.member ( a, b ) network.fetchedEdges |> not
    , Set.member ( b, a ) network.fetchedEdges |> not
    )


insertFetchedEdge : Direction -> Id -> Id -> Network -> Network
insertFetchedEdge dir id nid network =
    let
        pairByDir =
            case dir of
                Outgoing ->
                    ( id, nid )

                Incoming ->
                    ( nid, id )
    in
    { network | fetchedEdges = Set.insert pairByDir network.fetchedEdges }
