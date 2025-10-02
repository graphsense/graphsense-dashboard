module Update.Pathfinder exposing (deserialize, fetchTagSummaryForId, fromDeserialized, removeAddress, removeAggEdge, unselect, update, updateByPluginOutMsg, updateByRoute)

import Animation as A
import Api.Data
import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Components.InfiniteTable as InfiniteTable
import Config.Pathfinder exposing (TracingMode(..), nodeXOffset)
import Config.Update as Update
import Css.Pathfinder exposing (searchBoxMinWidth)
import Decode.Pathfinder1
import Dict exposing (Dict)
import Effect.Api as Api exposing (Effect(..))
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Hovercard
import Iknaio.ColorScheme exposing (annotationGreen, annotationRed)
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Pathfinder as Pathfinder
import Init.Pathfinder.AddressDetails as AddressDetails
import Init.Pathfinder.AggEdge as AggEdge
import Init.Pathfinder.ConversionDetails as ConversionDetails
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Init.Pathfinder.RelationDetails as RelationDetails
import Init.Pathfinder.TxDetails as TxDetails
import Json.Decode
import List.Extra
import Log
import Maybe.Extra
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, relativeToGraphZero)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale as Locale
import Model.Notification as Notification
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address as Address exposing (Address, Txs(..), expandAllowed, getTxs, txsSetter)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.AggEdge as AggEdge
import Model.Pathfinder.CheckingNeighbors as CheckingNeighbors
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.ConversionEdge as ConversionEdge
import Model.Pathfinder.Deserialize exposing (Deserialized)
import Model.Pathfinder.Error exposing (Error(..), InfoError(..))
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (FindPosition(..), Network)
import Model.Pathfinder.RelationDetails as RelationDetails
import Model.Pathfinder.Tools exposing (PointerTool(..), ToolbarHovercardType(..), toolbarHovercardTypeToId)
import Model.Pathfinder.Tooltip as Tooltip
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Search as Search
import Model.Tx as GTx exposing (parseTxIdentifier)
import Msg.Pathfinder
    exposing
        ( DisplaySettingsMsg(..)
        , Msg(..)
        , OverlayWindows(..)
        , TxDetailsMsg(..)
        )
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.ConversionDetails as ConversionDetails
import Msg.Pathfinder.RelationDetails as RelationDetails
import Msg.Search as Search
import Number.Bounded exposing (value)
import Plugin.Msg as Plugin
import Plugin.Update as Plugin exposing (Plugins)
import PluginInterface.Msg as PluginInterface
import Ports
import Process
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Route as GlobalRoute
import Route.Pathfinder as Route exposing (AddressHopType(..), PathHopType(..), Route)
import Set exposing (..)
import Task
import Time
import Tuple exposing (first, mapFirst, mapSecond, pair, second)
import Tuple2 exposing (pairTo)
import Tuple3
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Transform as Transform
import Update.Pathfinder.AddressDetails as AddressDetails
import Update.Pathfinder.AggEdge as AggEdge
import Update.Pathfinder.ConversionDetails as ConversionDetails
import Update.Pathfinder.Network as Network exposing (ingestAddresses, ingestAggEdges, ingestTxs)
import Update.Pathfinder.Node as Node
import Update.Pathfinder.RelationDetails as RelationDetails
import Update.Pathfinder.TxDetails as TxDetails
import Update.Pathfinder.WorkflowNextTxByTime as WorkflowNextTxByTime
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Update.Search as Search
import Util exposing (and, n, removeLeading0x)
import Util.Annotations as Annotations
import Util.Data as Data
import Util.EventualMessages as EventualMessages
import Util.Pathfinder.History as History
import Util.Pathfinder.TagSummary as TagSummary
import Util.Tag as Tag
import View.Locale as Locale
import Workflow


zoomFactor : Float
zoomFactor =
    0.5


getTagsummary : HavingTags -> Maybe Api.Data.TagSummary
getTagsummary ht =
    case ht of
        HasTagSummaryWithCluster ts ->
            Just ts

        HasTagSummaryWithoutCluster ts ->
            Just ts

        HasTagSummaryOnlyWithCluster ts ->
            Just ts

        HasTagSummaries { withCluster } ->
            Just withCluster

        _ ->
            Nothing


dispatchEventualMessages : Model -> ( Model, List Effect )
dispatchEventualMessages model =
    let
        ( updatedModel, cmds ) =
            EventualMessages.dispatchMessages model.network model.eventualMessages
    in
    ( { model | eventualMessages = updatedModel }
    , cmds |> Maybe.map (CmdEffect >> List.singleton) |> Maybe.withDefault []
    )


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory plugins msg
        |> markDirty plugins msg
        |> updateByMsg plugins uc msg
        |> and (syncSidePanel uc)
        |> and dispatchEventualMessages


syncSidePanel : Update.Config -> Model -> ( Model, List Effect )
syncSidePanel uc model =
    let
        makeAddressDetails aid =
            Dict.get aid model.network.addresses
                |> Maybe.map AddressDetails.init
                |> Maybe.map (AddressDetails aid)

        makeTxDetails tid =
            Dict.get tid model.network.txs
                |> Maybe.map (TxDetails.init (uc.locale |> flip Locale.getTokenTickers (Id.network tid)) >> TxDetails tid)

        makeRelationDetails rid =
            Dict.get rid model.network.aggEdges
                |> Maybe.map (RelationDetails.init >> RelationDetails rid)
    in
    (case ( model.selection, model.details ) of
        ( SelectedAddress id, Just (AddressDetails aid _) ) ->
            if id == aid then
                model.details

            else
                makeAddressDetails id

        ( SelectedConversionEdge id, Just (ConversionDetails cid _) ) ->
            if id == cid then
                model.details

            else
                Dict.get id model.network.conversions
                    |> Maybe.map (ConversionDetails.init >> ConversionDetails id)

        ( SelectedConversionEdge id, _ ) ->
            Dict.get id model.network.conversions
                |> Maybe.map (ConversionDetails.init >> ConversionDetails id)

        ( SelectedAddress id, _ ) ->
            makeAddressDetails id

        ( SelectedTx id, Just (TxDetails tid _) ) ->
            if id == tid then
                model.details

            else
                makeTxDetails id

        ( SelectedTx id, _ ) ->
            makeTxDetails id

        ( SelectedAggEdge id, Just (RelationDetails tid _) ) ->
            if id == tid then
                model.details

            else
                makeRelationDetails id

        ( SelectedAggEdge id, _ ) ->
            makeRelationDetails id

        ( MultiSelect mops, details ) ->
            case ( List.reverse mops |> List.head, details ) of
                ( Just (MSelectedAddress id), Just (AddressDetails aid _) ) ->
                    if id == aid then
                        model.details

                    else
                        makeAddressDetails id

                ( Just (MSelectedAddress id), _ ) ->
                    makeAddressDetails id

                ( Just (MSelectedTx id), Just (TxDetails tid _) ) ->
                    if id == tid then
                        model.details

                    else
                        makeTxDetails id

                ( Just (MSelectedTx id), _ ) ->
                    makeTxDetails id

                _ ->
                    Nothing

        ( WillSelectTx _, _ ) ->
            model.details

        ( WillSelectAddress _, _ ) ->
            model.details

        ( WillSelectAggEdge _, _ ) ->
            model.details

        ( NoSelection, _ ) ->
            Nothing
    )
        |> Maybe.map
            (\details ->
                (case details of
                    ConversionDetails _ _ ->
                        n (Just details)

                    RelationDetails rid rd ->
                        Dict.get rid model.network.aggEdges
                            |> Maybe.map (flip s_aggEdge rd >> RelationDetails rid >> Just)
                            |> Maybe.withDefault Nothing
                            |> n

                    TxDetails tid td ->
                        case Dict.get tid model.network.txs of
                            Just tx ->
                                let
                                    newM =
                                        tx |> flip s_tx td
                                in
                                TxDetails.loadTxDetailsDataAccount tx newM
                                    |> mapFirst (TxDetails tid >> Just)

                            _ ->
                                n Nothing

                    AddressDetails aid ad ->
                        let
                            dateFilterPreset =
                                case model.route of
                                    Route.Network _ (Route.Address _ dateFilter) ->
                                        dateFilter

                                    _ ->
                                        Nothing
                        in
                        Dict.get aid model.network.addresses
                            |> Maybe.map
                                (AddressDetails.syncByAddress uc model.network model.clusters dateFilterPreset ad
                                    >> mapFirst (AddressDetails aid >> Just)
                                )
                            |> Maybe.withDefault (n Nothing)
                )
                    |> mapFirst (flip s_details model)
            )
        |> Maybe.withDefault (n { model | details = Nothing })


resultLineToRoute : Search.ResultLine -> Route
resultLineToRoute search =
    case search of
        Search.Address net address ->
            Route.Network net (Route.Address address Nothing)

        Search.Tx net h ->
            Route.Network net (Route.Tx h)

        Search.Block net b ->
            Route.Network net (Route.Block b)

        Search.Label s ->
            Route.Label s

        Search.Actor ( id, _ ) ->
            Route.Actor id

        Search.Custom _ ->
            Route.Root


updateByMsg : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
updateByMsg plugins uc msg model =
    case Log.truncate "msg" msg of
        ConversionDetailsMsg _ (ConversionDetails.UserClickedTxCheckboxInTable txId) ->
            addOrRemoveTx plugins Nothing txId model
                |> and (setTracingMode TransactionTracingMode)

        ConversionDetailsMsg _ smsg ->
            case model.details of
                Just (ConversionDetails cid cModel) ->
                    let
                        ( nm, eff ) =
                            ConversionDetails.update smsg cModel
                    in
                    ( { model | details = Just (ConversionDetails cid nm) }
                    , eff
                    )

                _ ->
                    n model

        EventualMessagesHeartBeat ->
            let
                ( nm, cmd ) =
                    EventualMessages.heartBeat model.eventualMessages
            in
            ( { model | eventualMessages = nm }, cmd |> Maybe.map (CmdEffect >> List.singleton) |> Maybe.withDefault [] )

        UserOpensDialogWindow windowType ->
            case windowType of
                TagsList id ->
                    ( model
                    , UserGotDataForTagsListDialog id
                        |> Api.GetAddressTagsEffect { currency = Id.network id, address = Id.id id, pagesize = 5000, nextpage = Nothing, includeBestClusterTag = True }
                        |> ApiEffect
                        |> List.singleton
                    )

                AddTags _ ->
                    -- Managed Upstream
                    n model

        UserGotDataForTagsListDialog _ _ ->
            -- handled in src/Update.elm
            n model

        RuntimePostponedUpdateByRoute route ->
            updateByRoute plugins uc route model

        PluginMsg _ ->
            -- handled in src/Update.elm
            n model

        UserClickedExportGraphAsImage _ ->
            -- handled in src/Update.elm
            n model

        UserClickedSaveGraph _ ->
            -- handled in src/Update.elm
            n model

        NoOp ->
            n model

        BrowserGotActor id data ->
            let
                isMatchingActor ( aid, a ) =
                    let
                        isMatching ts =
                            if ts.bestActor |> Maybe.map ((==) id) |> Maybe.withDefault False then
                                Just aid

                            else
                                Nothing
                    in
                    a |> getTagsummary |> Maybe.andThen isMatching

                idsToUpdate =
                    model.tagSummaries
                        |> Dict.toList
                        |> List.filterMap isMatchingActor
            in
            n
                (idsToUpdate
                    |> List.foldl (\addressId m -> updateTagDataOnAddress addressId m)
                        { model
                            | actors = Dict.insert id data model.actors
                        }
                )

        UserPressedModKey ->
            n { model | modPressed = True }

        UserReleasedModKey ->
            n { model | modPressed = False }

        UserReleasedEscape ->
            unselect model |> Tuple.mapFirst (s_details Nothing)

        UserReleasedDeleteKey ->
            deleteSelection model

        UserReleasedNormalKey key ->
            case ( model.modPressed, key ) of
                ( True, "z" ) ->
                    update plugins uc UserClickedUndo model

                ( True, "y" ) ->
                    update plugins uc UserClickedRedo model

                _ ->
                    n model

        UserPressedNormalKey _ ->
            n model

        BrowserGotAddressData { id, pos, autoLinkTxInTraceMode } data ->
            let
                condFetchEgonet =
                    fetchEgonet id autoLinkTxInTraceMode data
            in
            if pos == Auto && not (Network.isEmpty model.network) && Network.findAddressCoords id model.network == Nothing then
                let
                    ( newModel, eff ) =
                        condFetchEgonet model
                in
                if List.isEmpty eff then
                    browserGotAddressData uc plugins id Auto data newModel
                        |> and condFetchEgonet

                else
                    ( newModel, eff )

            else
                browserGotAddressData uc plugins id pos data model
                    |> and condFetchEgonet

        BrowserGotRelationsToVisibleNeighbors { id, dir, requestIds, autoLinkInTraceMode } { neighbors } ->
            let
                neighborIds =
                    neighbors
                        |> List.map (\{ address } -> Id.init address.currency address.address)

                nset =
                    Set.fromList neighborIds

                upd nid edge =
                    if edge.a == nid && dir == Outgoing then
                        { edge
                            | b2a =
                                if RemoteData.isSuccess edge.b2a then
                                    edge.b2a

                                else
                                    Success Nothing
                        }

                    else if edge.a == nid && dir == Incoming then
                        { edge
                            | a2b =
                                if RemoteData.isSuccess edge.a2b then
                                    edge.a2b

                                else
                                    Success Nothing
                        }

                    else if edge.b == nid && dir == Outgoing then
                        { edge
                            | a2b =
                                if RemoteData.isSuccess edge.a2b then
                                    edge.a2b

                                else
                                    Success Nothing
                        }

                    else if edge.b == nid && dir == Incoming then
                        { edge
                            | b2a =
                                if RemoteData.isSuccess edge.b2a then
                                    edge.b2a

                                else
                                    Success Nothing
                        }

                    else
                        edge

                newModel =
                    neighbors
                        |> List.foldl
                            (\neighbor mo ->
                                mo.network
                                    |> Network.upsertAggEdgeData model.config id dir neighbor
                                    |> flip s_network mo
                            )
                            (CheckingNeighbors.insert dir id neighbors model.checkingNeighbors
                                |> flip s_checkingNeighbors model
                            )

                isEmpty =
                    -- both directions have been checked and there are no neighbors
                    CheckingNeighbors.isEmpty id newModel.checkingNeighbors

                newModel2 =
                    requestIds
                        |> List.filter (flip Set.member nset >> not)
                        |> List.foldl
                            (\nid ->
                                upd nid
                                    |> Network.rupsertAggEdge model.config (AggEdge.initId id nid)
                            )
                            newModel.network
                        |> flip s_network newModel
            in
            if newModel2.config.tracingMode == AggregateTracingMode && not isEmpty then
                CheckingNeighbors.getData id newModel2.checkingNeighbors
                    |> Maybe.map2
                        (\nid data ->
                            newModel2.checkingNeighbors
                                |> CheckingNeighbors.removeAll id
                                |> flip s_checkingNeighbors newModel2
                                |> browserGotAddressData uc plugins id (NextTo ( Direction.flip dir, nid )) data
                        )
                        (List.head neighborIds)
                    |> Maybe.withDefault (n newModel2)

            else if isEmpty then
                CheckingNeighbors.getData id model.checkingNeighbors
                    -- in newModel2 it's already empty
                    |> Maybe.map
                        (\data ->
                            browserGotAddressData uc plugins id Auto data newModel2
                        )
                    |> Maybe.withDefault (n newModel2)

            else
                neighbors
                    |> List.concatMap
                        (\nbr ->
                            let
                                isToLargeToLoadLinks a b =
                                    let
                                        maxTxs =
                                            10000

                                        noTx adr =
                                            adr.noIncomingTxs + adr.noOutgoingTxs
                                    in
                                    noTx a > maxTxs && noTx b > maxTxs

                                addressId =
                                    Id.init nbr.address.currency nbr.address.address

                                nbrData =
                                    nbr.address

                                aData =
                                    CheckingNeighbors.getData id model.checkingNeighbors

                                loadBetweenLinks =
                                    aData
                                        |> Maybe.map (flip isToLargeToLoadLinks nbrData >> not)
                                        |> Maybe.withDefault False

                                txs =
                                    Dict.get (AggEdge.initId id addressId) newModel2.network.aggEdges
                                        |> Maybe.map .txs
                                        |> Maybe.withDefault Set.empty
                            in
                            if model.config.tracingMode == TransactionTracingMode && Set.isEmpty txs then
                                getNextTxEffects newModel2.network
                                    addressId
                                    (Direction.flip dir)
                                    { addBetweenLinks = autoLinkInTraceMode && loadBetweenLinks, addAnyLinks = autoLinkInTraceMode }
                                    (Just id)

                            else
                                []
                        )
                    |> pair newModel2

        BrowserGotClusterData addressId data ->
            let
                clusterId =
                    Id.initClusterId data.currency data.entity
            in
            n
                { model
                    | clusters = Dict.insert clusterId (Success data) model.clusters
                }

        SearchMsg m ->
            case m of
                Search.UserClicksResultLine ->
                    let
                        query =
                            Search.query model.search

                        selectedValue =
                            Search.selectedValue model.search

                        ( search, _ ) =
                            Search.update m model.search

                        m2 =
                            { model | search = search }
                    in
                    if String.isEmpty query then
                        n m2

                    else
                        case selectedValue of
                            Just value ->
                                value
                                    |> resultLineToRoute
                                    |> NavPushRouteEffect
                                    |> List.singleton
                                    |> Tuple.pair m2

                            Nothing ->
                                n m2

                _ ->
                    Search.update m model.search
                        |> Tuple.mapFirst (\s -> s_search s model)
                        |> Tuple.mapSecond (List.map Pathfinder.SearchEffect)

        UserClosedDetailsView ->
            { model | details = Nothing, selection = NoSelection }
                |> n

        TxDetailsMsg (UserClickedTxInSubTxsTable tx) ->
            addOrRemoveTx plugins (Just (Id.init tx.network tx.fromAddress)) (Id.init tx.network tx.identifier) model
                |> and (setTracingMode TransactionTracingMode)

        TxDetailsMsg submsg ->
            case model.details of
                Just (TxDetails id txViewState) ->
                    let
                        ( nVs, eff ) =
                            TxDetails.update submsg txViewState
                    in
                    ( { model | details = Just (TxDetails id nVs) }, eff )

                _ ->
                    n model

        RelationDetailsMsg id submsg ->
            let
                addrA =
                    Dict.get (Tuple.first id) model.network.addresses

                addrB =
                    Dict.get (Tuple.second id) model.network.addresses

                ar1 =
                    addrA
                        |> Maybe.andThen Address.getActivityRangeAddress
                        |> Maybe.map (Tuple.mapBoth Time.posixToMillis Time.posixToMillis)
                        |> Maybe.withDefault ( 0, 0 )

                ar2 =
                    addrB
                        |> Maybe.andThen Address.getActivityRangeAddress
                        |> Maybe.map (Tuple.mapBoth Time.posixToMillis Time.posixToMillis)
                        |> Maybe.withDefault ( 0, 0 )

                ar =
                    ( Time.millisToPosix (min (Tuple.first ar1) (Tuple.first ar2)), Time.millisToPosix (max (Tuple.second ar1) (Tuple.second ar2)) )
            in
            (case submsg of
                RelationDetails.UserClickedTxCheckboxInTable tx ->
                    addOrRemoveTx plugins Nothing (Tx.getTxIdForRelationTx tx) model
                        |> and (setTracingMode TransactionTracingMode)

                RelationDetails.UserClickedTx txId ->
                    userClickedTx txId model
                        |> and (setTracingMode TransactionTracingMode)

                RelationDetails.UserClickedAllTxCheckboxInTable isA2b ->
                    getRelationDetails model id
                        |> Maybe.map
                            (\rdModel ->
                                let
                                    gs =
                                        RelationDetails.gettersAndSetters isA2b
                                in
                                gs.getTable rdModel
                                    |> .table
                                    |> InfiniteTable.getPage
                                    |> List.map Tx.getTxIdForRelationTx
                                    |> flip (checkAllTxs plugins uc Nothing) model
                                    |> and (setTracingMode TransactionTracingMode)
                            )
                        |> Maybe.withDefault (n model)

                _ ->
                    n model
            )
                |> and (updateRelationDetails uc id ar submsg)

        AddressDetailsMsg addressId subm ->
            let
                fetchTagSummariesForNeigbors neighbors =
                    let
                        network =
                            Id.network addressId
                    in
                    neighbors
                        |> List.map (.address >> .address >> Id.init network)
                        |> fetchTagSummaryForIds False model.tagSummaries
                        |> List.singleton
                        |> pair model
                        |> and
                            (AddressDetails.update uc subm
                                |> updateAddressDetails addressId
                            )
            in
            case subm of
                AddressDetails.GotNeighborsForAddressDetails _ { neighbors } ->
                    fetchTagSummariesForNeigbors neighbors

                AddressDetails.GotNeighborsNextPageForAddressDetails _ { neighbors } ->
                    fetchTagSummariesForNeigbors neighbors

                AddressDetails.BrowserGotAddressesForTags _ addresses ->
                    let
                        network =
                            Id.network addressId
                    in
                    addresses
                        |> List.map (.address >> Id.init network)
                        |> fetchTagSummaryForIds False model.tagSummaries
                        |> List.singleton
                        |> pair model
                        |> and
                            (AddressDetails.update uc subm
                                |> updateAddressDetails addressId
                            )

                AddressDetails.UserClickedAddressCheckboxInTable id ->
                    userClickedAddressCheckboxInTable plugins id model

                AddressDetails.UserClickedAggEdgeCheckboxInTable dir anchorId data ->
                    userClickedAggEdgeCheckboxInTable plugins dir anchorId data model

                AddressDetails.UserClickedAllTxCheckboxInTable ->
                    case model.details of
                        Just (AddressDetails _ data) ->
                            data.txs
                                |> RemoteData.map
                                    (.table
                                        >> InfiniteTable.getPage
                                        >> List.map Tx.getTxIdForAddressTx
                                        >> flip (checkAllTxs plugins uc (Just addressId)) model
                                    )
                                |> RemoteData.withDefault (n model)

                        _ ->
                            n model

                AddressDetails.UserClickedTxCheckboxInTable tx ->
                    addOrRemoveTx plugins (Just addressId) (Tx.getTxIdForAddressTx tx) model

                AddressDetails.UserClickedTx id ->
                    userClickedTx id model

                AddressDetails.TooltipMsg tm ->
                    handleTooltipMsg tm model

                _ ->
                    model
                        |> updateAddressDetails addressId
                            (AddressDetails.update uc subm)

        UserClickedRestart ->
            -- Handled upstream
            n model

        UserClickedRestartYes ->
            -- Handled upstream
            n model

        UserClickedUndo ->
            undoRedo History.undo model

        UserClickedRedo ->
            undoRedo History.redo model

        UserClickedGraph dragging ->
            let
                click =
                    case dragging of
                        NoDragging ->
                            True

                        Dragging _ start current ->
                            draggingToClick start current

                        DraggingNode _ start current ->
                            draggingToClick start current

                m1 =
                    model
                        |> s_toolbarHovercard Nothing
                        |> s_contextMenu Nothing
            in
            if click then
                ( m1
                , Route.Root
                    |> NavPushRouteEffect
                    |> List.singleton
                )
                    |> and unselect

            else
                unselect m1

        UserClickedFitGraph ->
            fitGraph uc model
                |> n

        BrowserWaitedAfterReleasingMouseButton ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging tm start now ->
                    case model.pointerTool of
                        Select ->
                            let
                                xoffset =
                                    searchBoxMinWidth / 2

                                crd =
                                    case tm.state of
                                        Transform.Settled c ->
                                            c

                                        Transform.Transitioning v ->
                                            v.from

                                z =
                                    value crd.z

                                xn =
                                    ((Basics.min start.x now.x + xoffset) * z) + crd.x

                                yn =
                                    (Basics.min start.y now.y * z) + crd.y

                                widthn =
                                    abs (start.x - now.x) * z

                                heightn =
                                    abs (start.y - now.y) * z

                                isIn x1 y1 x2 y2 x y =
                                    x > x1 && x < x2 && y > y1 && y < y2

                                isinRect c =
                                    isIn xn yn (xn + widthn) (yn + heightn) (c.x * unit) (c.y * unit)

                                isinRectTx tx =
                                    tx |> Tx.getCoords |> Maybe.map isinRect |> Maybe.withDefault False

                                isinRectAddr adr =
                                    adr |> Address.getCoords |> isinRect

                                selectedTxs =
                                    List.filter isinRectTx (Dict.values model.network.txs) |> List.map (.id >> MSelectedTx)

                                selectedAdr =
                                    List.filter isinRectAddr (Dict.values model.network.addresses) |> List.map (.id >> MSelectedAddress)

                                modelS =
                                    multiSelect model (selectedTxs ++ selectedAdr) False
                            in
                            n
                                { modelS
                                    | dragging = NoDragging
                                    , pointerTool = Drag
                                }

                        Drag ->
                            n model

                DraggingNode _ _ _ ->
                    n model

        UserReleasesMouseButton ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging _ _ _ ->
                    case model.pointerTool of
                        Select ->
                            ( model
                            , Process.sleep 0
                                |> Task.perform (\_ -> BrowserWaitedAfterReleasingMouseButton)
                                |> CmdEffect
                                |> List.singleton
                            )

                        Drag ->
                            n
                                { model
                                    | dragging = NoDragging
                                }

                DraggingNode id _ _ ->
                    let
                        moveNode txOrAdrId net =
                            Network.updateAddress txOrAdrId Node.release net
                                |> Network.updateTx txOrAdrId
                                    Node.release

                        moveSelectedNode sel net =
                            case sel of
                                MSelectedAddress aid ->
                                    moveNode aid net

                                MSelectedTx tid ->
                                    moveNode tid net

                        network =
                            case model.selection of
                                MultiSelect sel ->
                                    List.foldl moveSelectedNode model.network sel

                                _ ->
                                    moveNode id model.network

                        nn =
                            if model.config.snapToGrid then
                                network |> Network.snapToGrid

                            else
                                network
                    in
                    n
                        { model
                            | network = nn
                            , dragging = NoDragging
                        }

        UserWheeledOnGraph x y z ->
            uc.size
                |> Maybe.map
                    (\size ->
                        { model
                            | transform =
                                Transform.wheel
                                    { width = size.width
                                    , height = size.height
                                    }
                                    x
                                    y
                                    (z * zoomFactor)
                                    model.transform
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserPushesLeftMouseButtonOnGraph coords ->
            ( { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            Dragging model.transform (relativeToGraphZero uc.size coords) (relativeToGraphZero uc.size coords)

                        _ ->
                            NoDragging
              }
            , CloseTooltipEffect Nothing False |> List.singleton
            )

        UserPushesLeftMouseButtonOnAddress id coords ->
            ( { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            DraggingNode id coords coords

                        _ ->
                            model.dragging
              }
            , CloseTooltipEffect Nothing False |> List.singleton
            )

        UserMovesMouseOverTx id ->
            if model.hovered == HoveredTx id then
                n model

            else
                let
                    domId =
                        Id.toString id

                    hovered _ =
                        let
                            unhovered =
                                unhover model

                            maybeTT =
                                unhovered.network.txs
                                    |> Dict.get id
                                    |> Maybe.map
                                        (\tx ->
                                            let
                                                msgs =
                                                    { openTooltip = UserMovesMouseOverTx tx.id
                                                    , closeTooltip = UserMovesMouseOverTx tx.id
                                                    , openDetails = Nothing
                                                    }
                                            in
                                            case tx.type_ of
                                                Tx.Utxo t ->
                                                    Tooltip.UtxoTx t msgs

                                                Tx.Account t ->
                                                    Tooltip.AccountTx t msgs
                                        )
                        in
                        ( { unhovered
                            | network =
                                Network.updateTx id (s_hovered True) unhovered.network
                                    |> Network.trySetHoverConversionLoop id True
                            , hovered = HoveredTx id
                          }
                        , case maybeTT of
                            Just tt ->
                                OpenTooltipEffect { context = domId, domId = domId } False tt |> List.singleton

                            _ ->
                                []
                        )
                in
                case model.details of
                    Just (TxDetails txid _) ->
                        if id /= txid then
                            hovered ()

                        else
                            n model

                    _ ->
                        hovered ()

        UserMovesMouseOverAddress id ->
            if model.hovered == HoveredAddress id then
                n model

            else
                let
                    domId =
                        Id.toString id

                    showHover _ =
                        let
                            unhovered =
                                unhover model

                            maybeTT =
                                unhovered.network.addresses
                                    |> Dict.get id
                                    |> Maybe.map
                                        (\addr ->
                                            Tooltip.Address addr
                                                (case Dict.get id model.tagSummaries of
                                                    Just (HasTagSummaries { withCluster }) ->
                                                        Just withCluster

                                                    Just (HasTagSummaryWithCluster ts) ->
                                                        Just ts

                                                    _ ->
                                                        Nothing
                                                )
                                        )
                        in
                        ( { unhovered
                            | hovered = HoveredAddress id
                          }
                        , case maybeTT of
                            Just tt ->
                                OpenTooltipEffect { context = domId, domId = domId } False tt |> List.singleton

                            _ ->
                                []
                        )
                in
                case model.details of
                    Just (AddressDetails aid _) ->
                        if id /= aid then
                            showHover ()

                        else
                            n model

                    _ ->
                        showHover ()

        UserMovesMouseOutAddress id ->
            ( unhover model, CloseTooltipEffect (Just { context = Id.toString id, domId = Id.toString id }) False |> List.singleton )

        ShowTextTooltip config ->
            ( model, OpenTooltipEffect { context = config.domId, domId = config.domId } False (Tooltip.Text config.text) |> List.singleton )

        CloseTextTooltip config ->
            ( model, CloseTooltipEffect (Just { context = config.domId, domId = config.domId }) True |> List.singleton )

        UserMovesMouseOverTagLabel ctx ->
            let
                tsToTooltip ts =
                    let
                        tt =
                            Tooltip.TagLabel ctx.context
                                ts
                                { openTooltip = UserMovesMouseOverTagLabel ctx
                                , closeTooltip = UserMovesMouseOutTagLabel ctx
                                , openDetails = Nothing
                                }
                    in
                    ( model
                    , OpenTooltipEffect ctx False tt |> List.singleton
                    )
            in
            case model.details of
                Just (AddressDetails id _) ->
                    case Dict.get id model.tagSummaries of
                        Just (HasTagSummaries { withCluster }) ->
                            tsToTooltip withCluster

                        Just (HasTagSummaryOnlyWithCluster ts) ->
                            tsToTooltip ts

                        Just (HasTagSummaryWithCluster ts) ->
                            tsToTooltip ts

                        _ ->
                            n model

                _ ->
                    n model

        UserMovesMouseOverActorLabel ctx ->
            case Dict.get ctx.context model.actors of
                Just actor ->
                    let
                        tt =
                            Tooltip.ActorDetails actor
                                { openTooltip = UserMovesMouseOverActorLabel ctx
                                , closeTooltip = UserMovesMouseOutActorLabel ctx
                                , openDetails = Nothing
                                }
                    in
                    ( model
                    , OpenTooltipEffect ctx False tt
                        |> List.singleton
                    )

                _ ->
                    n model

        UserMovesMouseOutActorLabel ctx ->
            ( model, CloseTooltipEffect (Just ctx) True |> List.singleton )

        UserMovesMouseOutTagLabel ctx ->
            ( model, CloseTooltipEffect (Just ctx) True |> List.singleton )

        UserMovesMouseOutTx id ->
            ( unhover model
            , CloseTooltipEffect
                (Just { context = Id.toString id, domId = Id.toString id })
                False
                |> List.singleton
            )

        UserPushesLeftMouseButtonOnUtxoTx id coords ->
            ( { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            DraggingNode id coords coords

                        _ ->
                            model.dragging
              }
            , CloseTooltipEffect Nothing False |> List.singleton
            )

        UserMovesMouseOnGraph coords ->
            case model.dragging of
                NoDragging ->
                    ( model, CloseTooltipEffect Nothing False |> List.singleton )

                Dragging transform start _ ->
                    (case model.pointerTool of
                        Drag ->
                            { model
                                | transform = Transform.update start (relativeToGraphZero uc.size coords) transform
                                , dragging = Dragging transform start (relativeToGraphZero uc.size coords)
                            }

                        Select ->
                            { model
                                | dragging = Dragging transform start (relativeToGraphZero uc.size coords)
                            }
                    )
                        |> n

                DraggingNode id start _ ->
                    let
                        vector =
                            Transform.vector start coords model.transform

                        vectorRel =
                            { x = vector.x / unit
                            , y = vector.y / unit
                            }

                        moveNode txOrAdrId net =
                            Network.updateAddress txOrAdrId (Node.move vectorRel) net
                                |> Network.updateTx txOrAdrId
                                    (Node.move vectorRel)

                        moveSelectedNode sel net =
                            case sel of
                                MSelectedAddress aid ->
                                    moveNode aid net

                                MSelectedTx tid ->
                                    moveNode tid net

                        network =
                            case model.selection of
                                MultiSelect sel ->
                                    List.foldl moveSelectedNode model.network sel

                                _ ->
                                    moveNode id model.network
                    in
                    { model
                        | network = network
                        , dragging = DraggingNode id start coords
                    }
                        |> n

        AnimationFrameDeltaForTransform delta ->
            n
                { model
                    | transform = Transform.transition delta model.transform
                }

        AnimationFrameDeltaForMove delta ->
            ( { model
                | network =
                    Network.animateAddresses delta model.network
                        |> Network.animateTxs delta
              }
            , [ RepositionTooltipEffect ]
            )

        UserClickedAddressExpandHandle id direction ->
            Dict.get id model.network.addresses
                |> Maybe.map
                    (\address ->
                        if expandAllowed address then
                            expandAddress address direction model

                        else
                            ( model
                            , TxTracingThroughService id address.exchange
                                |> InfoError
                                |> ErrorEffect
                                |> List.singleton
                            )
                    )
                |> Maybe.withDefault (n model)

        UserClickedAddress id ->
            if model.modPressed then
                multiSelect model [ MSelectedAddress id ] True
                    |> n

            else
                ( model
                , Route.addressRoute
                    { network = Id.network id
                    , address = Id.id id
                    }
                    |> NavPushRouteEffect
                    |> List.singleton
                )

        UserClickedAddressCheckboxInTable id ->
            userClickedAddressCheckboxInTable plugins id model

        UserClickedAllAddressCheckboxInTable dir ->
            case model.details of
                Just (TxDetails _ data) ->
                    let
                        t =
                            case dir of
                                Incoming ->
                                    data.outputsTable

                                Outgoing ->
                                    data.inputsTable

                        network =
                            data.tx |> Tx.getNetwork

                        idsTable =
                            t.data
                                |> List.filterMap (Tx.ioToId network)

                        allChecked =
                            idsTable
                                |> List.all (flip Dict.member model.network.addresses)

                        deleteAcc aId ( m, eff ) =
                            removeAddress aId m |> Tuple.mapSecond ((++) eff)

                        addAcc aId ( m, eff ) =
                            loadAddress plugins True aId m |> Tuple.mapSecond ((++) eff)
                    in
                    if allChecked then
                        idsTable
                            |> List.filter (flip Dict.member model.network.addresses)
                            |> List.foldl deleteAcc (n model)

                    else
                        idsTable
                            |> List.filter (flip Dict.member model.network.addresses >> not)
                            |> List.foldl addAcc (n model)

                _ ->
                    n model

        UserClickedTx id ->
            userClickedTx id model

        UserClickedRemoveAddressFromGraph id ->
            removeAddress id model

        InternalConversionLoopAddressesLoaded conv ->
            let
                mtxInput =
                    ConversionEdge.getInputTransferIdRaw conv |> flip Dict.get model.network.txs

                mtxOutput =
                    ConversionEdge.getOutputTransferIdRaw conv |> flip Dict.get model.network.txs

                arrangeConversionNodes txInput txOutput =
                    let
                        moveNode v txOrAdrId net =
                            Network.updateAddress txOrAdrId (Node.moveAbs v) net

                        displacementFromTx =
                            4

                        --input leg
                        inputTxCoords =
                            txInput |> Tx.toFinalCoords

                        a =
                            txInput
                                |> Tx.getInputAddressIds
                                |> List.foldl (moveNode { x = inputTxCoords.x - displacementFromTx, y = inputTxCoords.y }) model.network

                        b =
                            txInput
                                |> Tx.getOutputAddressIds
                                |> List.foldl (moveNode { x = inputTxCoords.x + displacementFromTx, y = inputTxCoords.y }) a

                        -- output leg
                        outputTxCoords =
                            txOutput |> Tx.toFinalCoords

                        c =
                            txOutput
                                |> Tx.getOutputAddressIds
                                |> List.foldl (moveNode { x = outputTxCoords.x - displacementFromTx, y = outputTxCoords.y }) b

                        netOut =
                            txOutput
                                |> Tx.getInputAddressIds
                                |> List.foldl (moveNode { x = outputTxCoords.x + displacementFromTx, y = outputTxCoords.y }) c
                    in
                    n
                        { model
                            | network =
                                netOut
                                    |> (if model.config.snapToGrid then
                                            Network.snapToGrid

                                        else
                                            identity
                                       )
                        }
            in
            Maybe.map2 arrangeConversionNodes mtxInput mtxOutput
                |> Maybe.withDefault (n model)

        BrowserGotConversionLoop txA conversion tx ->
            let
                posA =
                    txA |> Tx.toFinalCoords

                ( ( ntx, nn ), newTx ) =
                    case Dict.get (Tx.getTxId tx) model.network.txs of
                        Just oldTx ->
                            ( ( oldTx, model.network ), False )

                        Nothing ->
                            ( Network.addTxWithPosition model.config (Fixed posA.x (posA.y + 2)) tx model.network, True )

                -- order txs such from and to, according to the which is the output leg (to) and which is the input leg (from)
                ( inputTx, outputTx ) =
                    if (txA.id |> Id.id |> removeLeading0x) == (conversion.fromAssetTransfer |> removeLeading0x) then
                        ( txA, ntx )

                    else
                        ( ntx, txA )

                nnn =
                    nn
                        |> Network.addConversion conversion inputTx outputTx
                        |> Network.updateTx inputTx.id (s_conversionType (Just Tx.InputLegConversion))
                        |> Network.updateTx outputTx.id (s_conversionType (Just Tx.OutputLegConversion))

                nFromAddress =
                    Data.normalizeIdentifier conversion.fromNetwork conversion.fromAddress

                nToAddress =
                    Data.normalizeIdentifier conversion.toNetwork conversion.toAddress

                eventualMsg =
                    Network.AndCondition
                        [ Network.AddressIsLoaded (Id.init conversion.fromNetwork nFromAddress)
                        , Network.AddressIsLoaded (Id.init conversion.toNetwork nToAddress)
                        , Network.OrCondition (inputTx |> Tx.getOutputAddressIds |> List.map Network.AddressIsLoaded)
                        , Network.OrCondition (outputTx |> Tx.getInputAddressIds |> List.map Network.AddressIsLoaded)
                        ]

                ( eventualMessagesNew, mcmd ) =
                    model.eventualMessages
                        |> EventualMessages.addMessage eventualMsg (InternalConversionLoopAddressesLoaded conversion)
            in
            (model
                |> s_network nnn
                |> s_eventualMessages eventualMessagesNew
            )
                |> checkSelection uc
                |> and
                    (if newTx then
                        autoLoadAddresses plugins False ntx

                     else
                        n
                    )
                |> Tuple.mapSecond ((++) (mcmd |> Maybe.map (CmdEffect >> List.singleton) |> Maybe.withDefault []))

        BrowserGotConversions tx conversions ->
            let
                txid =
                    Tx.getTxIdForTx tx

                supportedConversions =
                    conversions
                        |> List.filter (\c -> c.toIsSupportedAsset && c.fromIsSupportedAsset)
            in
            supportedConversions
                |> List.foldl
                    (\conversion ( aggm, effects ) ->
                        let
                            secondTransferId =
                                if Id.network txid == conversion.toNetwork && (Id.id txid |> removeLeading0x) == (conversion.toAssetTransfer |> removeLeading0x) then
                                    Id.init conversion.fromNetwork conversion.fromAssetTransfer

                                else
                                    Id.init conversion.toNetwork conversion.toAssetTransfer

                            effs =
                                BrowserGotConversionLoop tx conversion
                                    |> Api.GetTxEffect
                                        { currency = Id.network secondTransferId
                                        , txHash = Id.id secondTransferId
                                        , includeIo = True
                                        , tokenTxId = Nothing
                                        }
                                    |> ApiEffect
                                    |> List.singleton
                        in
                        ( aggm, effects ++ effs )
                    )
                    ( model, [] )

        BrowserGotTx { pos, loadAddresses, autoLinkInTraceMode } tx ->
            if Dict.member (Tx.getTxId tx) model.network.txs then
                n model

            else
                let
                    ( newTx, newNetwork ) =
                        Network.addTxWithPosition model.config pos tx model.network
                in
                (model |> s_network newNetwork)
                    |> checkSelection uc
                    |> and
                        (if loadAddresses then
                            autoLoadAddresses plugins autoLinkInTraceMode newTx

                         else
                            n
                        )
                    |> and (autoLoadConversions plugins newTx)

        UserClickedSelectionTool ->
            n
                { model
                    | pointerTool =
                        if model.pointerTool == Select then
                            Drag

                        else
                            Select
                }

        ChangedDisplaySettingsMsg submsg ->
            case submsg of
                UserClickedToggleSnapToGrid ->
                    -- handled Upstream
                    n model

                UserClickedToggleShowTxTimestamp ->
                    -- handled Upstream
                    n model

                UserClickedToggleDatesInUserLocale ->
                    -- handled Upstream
                    n model

                UserClickedToggleShowTimeZoneOffset ->
                    -- handled Upstream
                    n model

                UserClickedToggleValueDisplay ->
                    -- handled Upstream
                    n model

                UserClickedToggleValueDetail ->
                    -- handled Upstream
                    n model

                UserClickedToggleHighlightClusterFriends ->
                    -- handled Upstream
                    n model

                UserClickedToggleDisplaySettings ->
                    let
                        choosenHc =
                            Settings

                        nhcm =
                            toolbarHovercardTypeToId choosenHc
                                |> Hovercard.init
                                |> mapFirst (\hcm -> model |> s_toolbarHovercard (Just ( choosenHc, hcm )))
                                |> mapSecond
                                    (Cmd.map
                                        ToolbarHovercardMsg
                                        >> CmdEffect
                                        >> List.singleton
                                    )
                    in
                    case model.toolbarHovercard of
                        Nothing ->
                            nhcm

                        Just ( Settings, _ ) ->
                            n { model | toolbarHovercard = Nothing }

                        _ ->
                            nhcm

        ToolbarHovercardMsg hcMsg ->
            model.toolbarHovercard
                |> Maybe.map
                    (\( hovercardId, hc ) ->
                        Hovercard.update hcMsg hc
                            |> mapFirst (\hcm -> model |> s_toolbarHovercard (Just ( hovercardId, hcm )))
                            |> mapSecond
                                (Cmd.map
                                    ToolbarHovercardMsg
                                    >> CmdEffect
                                    >> List.singleton
                                )
                    )
                |> Maybe.withDefault (n model)

        UserToggleAnnotationSettings ->
            let
                choosenHc =
                    Annotation

                nhcm =
                    toolbarHovercardTypeToId choosenHc
                        |> Hovercard.init
                        |> mapFirst (\hcm -> model |> s_toolbarHovercard (Just ( choosenHc, hcm )))
                        |> mapSecond
                            (Cmd.map
                                ToolbarHovercardMsg
                                >> CmdEffect
                                >> List.singleton
                            )
            in
            case model.toolbarHovercard of
                Nothing ->
                    nhcm

                Just ( Annotation, _ ) ->
                    n { model | toolbarHovercard = Nothing }

                _ ->
                    nhcm

        UserOpensTxAnnotationDialog id ->
            let
                ( mn, effn ) =
                    selectTx id model

                ( resultModel, eff ) =
                    toolbarHovercardTypeToId Annotation
                        |> Hovercard.init
                        |> mapFirst (\hcm -> mn |> s_toolbarHovercard (Just ( Annotation, hcm )))
                        |> mapSecond
                            (Cmd.map
                                ToolbarHovercardMsg
                                >> CmdEffect
                                >> List.singleton
                            )
            in
            ( resultModel, effn ++ eff ++ [ Task.attempt (\_ -> NoOp) (Dom.focus "annotation-label-textbox") |> CmdEffect ] )

        UserOpensAddressAnnotationDialog id ->
            let
                ( mn, effn ) =
                    selectAddress id model

                ( resultModel, eff ) =
                    toolbarHovercardTypeToId Annotation
                        |> Hovercard.init
                        |> mapFirst (\hcm -> mn |> s_toolbarHovercard (Just ( Annotation, hcm )))
                        |> mapSecond
                            (Cmd.map
                                ToolbarHovercardMsg
                                >> CmdEffect
                                >> List.singleton
                            )
            in
            ( resultModel, effn ++ eff ++ [ Task.attempt (\_ -> NoOp) (Dom.focus "annotation-label-textbox") |> CmdEffect ] )

        WorkflowNextUtxoTx config neighborId wm ->
            WorkflowNextUtxoTx.update config wm
                |> flip (handleWorkflowNextUtxo plugins uc config neighborId) model

        WorkflowNextTxByTime config neighborId wm ->
            WorkflowNextTxByTime.update config wm
                |> flip (handleWorkflowNextTxByTime plugins uc config neighborId) model

        BrowserGotTagSummaries includesBestClusterTag data ->
            let
                combine ( id, ts ) r =
                    addTagSummaryToModel r includesBestClusterTag id ts
            in
            List.foldl combine (n model) data

        BrowserGotTagSummary includesBestClusterTag id data ->
            addTagSummaryToModel (n model) includesBestClusterTag id data

        BrowserGotAddressesTags _ data ->
            let
                isExchange =
                    (==) (Just TagSummary.exchangeCategory)

                updateHasTags ( id, tag ) =
                    Dict.update id
                        (Maybe.map
                            (\curr ->
                                case ( curr, tag ) of
                                    ( HasTagSummaries _, _ ) ->
                                        curr

                                    ( HasTagSummaryWithCluster _, _ ) ->
                                        curr

                                    ( HasTagSummaryWithoutCluster _, _ ) ->
                                        curr

                                    ( HasTagSummaryOnlyWithCluster _, _ ) ->
                                        curr

                                    ( NoTagsWithoutCluster, _ ) ->
                                        curr

                                    ( HasTags withExchangeTag, Just { category } ) ->
                                        withExchangeTag
                                            || isExchange category
                                            |> HasTags

                                    ( _, Just { category } ) ->
                                        if isExchange category then
                                            HasExchangeTagOnly

                                        else
                                            curr
                                                == HasExchangeTagOnly
                                                |> HasTags

                                    ( LoadingTags, Nothing ) ->
                                        NoTags

                                    ( NoTags, Nothing ) ->
                                        NoTags

                                    ( _, Nothing ) ->
                                        curr
                            )
                        )

                tagSummaries =
                    data
                        |> List.foldl updateHasTags model.tagSummaries
            in
            ( { model
                | tagSummaries = tagSummaries
              }
            , []
            )

        UserClickedToolbarDeleteIcon ->
            deleteSelection model

        UserClickedContextMenuDeleteIcon menuType ->
            case menuType of
                ContextMenu.AddressContextMenu id ->
                    removeAddress id model

                ContextMenu.TransactionContextMenu id ->
                    removeTx id model

                ContextMenu.AddressIdChevronActions _ ->
                    n model

                ContextMenu.TransactionIdChevronActions _ ->
                    n model

        BrowserGotBulkAddresses addresses ->
            addresses
                |> List.foldl
                    (\address mod ->
                        and (browserGotAddressData uc plugins (Id.init address.currency address.address) Auto address) mod
                    )
                    (n model)

        BrowserGotBulkTxs deserializing txs ->
            let
                updatedModel =
                    { model | network = ingestTxs model.config model.network deserializing.deserialized.txs txs }
            in
            -- Load Conversion Edges for new loaded transactions
            updatedModel.network.txs
                |> Dict.values
                |> List.foldl (\tx acc -> acc |> and (autoLoadConversions plugins tx)) ( updatedModel, [] )

        UserClickedOpenGraph ->
            ( model
            , Ports.deserialize ()
                |> CmdEffect
                |> List.singleton
            )

        UserInputsAnnotation id str ->
            n { model | annotations = Annotations.setLabel id str model.annotations }

        UserSelectsAnnotationColor id clr ->
            n { model | annotations = Annotations.setColor id clr model.annotations }

        UserOpensContextMenu coordsNew cmtype ->
            case model.contextMenu of
                Nothing ->
                    n { model | contextMenu = Just ( coordsNew, cmtype ) }

                Just ( coords, type_ ) ->
                    let
                        distance =
                            sqrt
                                ((coords.x - coordsNew.x) ^ 2 + (coords.y - coordsNew.y) ^ 2)
                    in
                    if ContextMenu.isContextMenuTypeEqual type_ cmtype && distance < 50.0 then
                        n { model | contextMenu = Nothing }
                        -- close on second click

                    else
                        n { model | contextMenu = Just ( coordsNew, cmtype ) }

        UserClosesContextMenu ->
            n { model | contextMenu = Nothing, helpDropdownOpen = False }

        UserClickedShowLegend ->
            n model

        UserClickedToggleHelpDropdown ->
            n (model |> s_helpDropdownOpen (model.helpDropdownOpen |> not))

        UserClickedContextMenuOpenInNewTab cm ->
            ( model
            , (case cm of
                ContextMenu.AddressContextMenu id ->
                    Route.Network (Id.network id) (Route.Address (Id.id id) Nothing)

                ContextMenu.TransactionContextMenu id ->
                    Route.Network (Id.network id) (Route.Tx (Id.id id))

                ContextMenu.TransactionIdChevronActions id ->
                    Route.Network (Id.network id) (Route.Tx (Id.id id))

                ContextMenu.AddressIdChevronActions id ->
                    Route.Network (Id.network id) (Route.Address (Id.id id) Nothing)
              )
                |> GlobalRoute.pathfinderRoute
                |> GlobalRoute.toUrl
                |> Ports.newTab
                |> CmdEffect
                |> List.singleton
            )

        UserClickedContextMenuIdToClipboard cm ->
            ( model
            , (case cm of
                ContextMenu.AddressContextMenu id ->
                    Id.id id

                ContextMenu.TransactionContextMenu id ->
                    case Id.id id |> parseTxIdentifier of
                        Nothing ->
                            Id.id id

                        Just (GTx.External hash) ->
                            hash

                        Just (GTx.Internal hash _) ->
                            hash

                        Just (GTx.Token hash _) ->
                            hash

                ContextMenu.TransactionIdChevronActions id ->
                    case Id.id id |> parseTxIdentifier of
                        Nothing ->
                            Id.id id

                        Just (GTx.External hash) ->
                            hash

                        Just (GTx.Internal hash _) ->
                            hash

                        Just (GTx.Token hash _) ->
                            hash

                ContextMenu.AddressIdChevronActions id ->
                    Id.id id
              )
                |> Ports.toClipboard
                |> CmdEffect
                |> List.singleton
            )

        UserClickedToggleTracingMode ->
            (case model.config.tracingMode of
                TransactionTracingMode ->
                    AggregateTracingMode

                AggregateTracingMode ->
                    TransactionTracingMode
            )
                |> flip setTracingMode model

        InternalPathfinderAddedAddress _ ->
            -- handled upstream
            n model

        UserClickedConversionEdge id _ ->
            model
                |> selectConversionEdge id

        UserMovesMouseOverConversionEdge id conv ->
            if model.hovered == HoveredConversionEdge id then
                n model

            else
                let
                    unhovered =
                        unhover model

                    txid1 =
                        ConversionEdge.getInputTransferId conv

                    txid2 =
                        ConversionEdge.getOutputTransferId conv
                in
                ( { unhovered
                    | network =
                        Network.updateConversionEdge id (s_hovered True) unhovered.network
                            |> Network.updateTx txid1 (s_hovered True)
                            |> Network.updateTx txid2 (s_hovered True)
                    , hovered = HoveredConversionEdge id
                  }
                , []
                )

        UserMovesMouseOutConversionEdge id conv ->
            let
                uhm =
                    unhover model

                newModel =
                    { uhm
                        | network =
                            uhm.network
                                |> Network.updateTx (ConversionEdge.getOutputTransferId conv) (s_hovered False)
                                |> Network.updateTx (ConversionEdge.getInputTransferId conv) (s_hovered False)
                    }
            in
            ( case model.selection of
                SelectedConversionEdge selId ->
                    if selId == id then
                        model

                    else
                        newModel

                _ ->
                    newModel
            , CloseTooltipEffect
                (Just { context = AggEdge.idToString id, domId = AggEdge.idToString id })
                False
                |> List.singleton
            )

        UserClickedAggEdge id ->
            ( model
            , Route.aggEdgeRoute
                { network = Id.network <| first id
                , a = Id.id <| first id
                , b = Id.id <| second id
                }
                |> NavPushRouteEffect
                |> List.singleton
            )

        UserMovesMouseOverAggEdge id ->
            if model.hovered == HoveredAggEdge id then
                n model

            else
                let
                    domId =
                        AggEdge.idToString id

                    hovered _ =
                        let
                            unhovered =
                                unhover model
                        in
                        ( { unhovered
                            | network = Network.updateAggEdge id (s_hovered True) unhovered.network
                            , hovered = HoveredAggEdge id
                          }
                        , unhovered.network.aggEdges
                            |> Dict.get id
                            |> Maybe.andThen
                                (\edge ->
                                    Maybe.map4
                                        (\a b a2b b2a ->
                                            if a.x < b.x then
                                                { leftAddress = a.id
                                                , left = a2b
                                                , rightAddress = b.id
                                                , right = b2a
                                                }

                                            else
                                                { leftAddress = b.id
                                                , left = b2a
                                                , rightAddress = a.id
                                                , right = a2b
                                                }
                                        )
                                        edge.aAddress
                                        edge.bAddress
                                        (RemoteData.toMaybe edge.a2b)
                                        (RemoteData.toMaybe edge.b2a)
                                )
                            |> Maybe.map
                                (flip Tooltip.AggEdge
                                    { openTooltip = UserMovesMouseOverAggEdge id
                                    , closeTooltip = UserMovesMouseOutAggEdge id
                                    , openDetails = Nothing
                                    }
                                )
                            |> Maybe.map (OpenTooltipEffect { context = domId, domId = domId } False)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                        )
                in
                case model.details of
                    Just (RelationDetails rid _) ->
                        if id /= rid then
                            hovered ()

                        else
                            n model

                    _ ->
                        hovered ()

        UserMovesMouseOutAggEdge id ->
            ( unhover model
            , CloseTooltipEffect
                (Just { context = AggEdge.idToString id, domId = AggEdge.idToString id })
                False
                |> List.singleton
            )


checkAllTxs : Plugins -> Update.Config -> Maybe Id -> List Id -> Model -> ( Model, List Effect )
checkAllTxs plugins uc addressId txIds model =
    let
        allChecked =
            txIds
                |> List.all (flip Dict.member model.network.txs)

        addOrRemoveAcc txId =
            and (addOrRemoveTx plugins addressId txId)

        notify message =
            String.fromInt
                >> List.singleton
                >> Locale.interpolated uc.locale message
                >> Notification.infoDefault
                >> Notification.map (s_isEphemeral True)
                >> Notification.map (s_showClose False)
                >> ShowNotificationEffect
                >> List.singleton
    in
    if allChecked then
        txIds
            |> List.foldl addOrRemoveAcc ( model, [] )
            |> (\( newModel, eff ) ->
                    ( newModel
                    , txIds
                        |> List.filter (flip Dict.member newModel.network.txs >> not)
                        |> List.length
                        |> notify "Removed {0} transactions"
                        |> (++) eff
                    )
               )

    else
        let
            toAdd =
                txIds
                    |> List.filter (flip Dict.member model.network.txs >> not)
        in
        toAdd
            |> List.foldl addOrRemoveAcc
                ( model
                , List.length toAdd
                    |> notify "Added {0} transactions"
                )


setTracingMode : TracingMode -> Model -> ( Model, List Effect )
setTracingMode tm model =
    s_tracingMode tm model.config
        |> flip s_config model
        |> n


updateRelationDetails : Update.Config -> ( Id, Id ) -> ( Time.Posix, Time.Posix ) -> RelationDetails.Msg -> Model -> ( Model, List Effect )
updateRelationDetails uc id activityRange msg model =
    getRelationDetails model id
        |> Maybe.map
            (\rdModel ->
                let
                    ( nVs, eff ) =
                        RelationDetails.update uc id activityRange msg rdModel
                in
                ( { model | details = Just (RelationDetails id nVs) }, eff )
            )
        |> Maybe.withDefault (n model)


getRelationDetails : Model -> ( Id, Id ) -> Maybe RelationDetails.Model
getRelationDetails model id =
    case model.details of
        Just (RelationDetails rId rdModel) ->
            if rId /= id then
                Nothing

            else
                Just rdModel

        _ ->
            Nothing


fetchEgonet : Id -> Bool -> Api.Data.Address -> Model -> ( Model, List Effect )
fetchEgonet id autoLinkInTraceMode data model =
    let
        ( outOnlyIds, incOnlyIds ) =
            model.network.addresses
                |> Dict.keys
                |> List.foldl
                    (\aId ( out, inc ) ->
                        if (aId == id) || (Id.network aId /= Id.network id) then
                            ( out, inc )

                        else
                            case Network.aggEdgeNeedsData id aId model.network of
                                ( True, True ) ->
                                    ( aId :: out
                                    , aId :: inc
                                    )

                                ( True, False ) ->
                                    ( aId :: out
                                    , inc
                                    )

                                ( False, True ) ->
                                    ( out
                                    , aId :: inc
                                    )

                                ( False, False ) ->
                                    ( out
                                    , inc
                                    )
                    )
                    ( [], [] )
    in
    if List.isEmpty outOnlyIds && List.isEmpty incOnlyIds then
        n model

    else
        let
            nw =
                incOnlyIds
                    |> List.foldl
                        (\nid ->
                            Network.updateAggEdge
                                (AggEdge.initId id nid)
                                (AggEdge.setLoading Incoming id)
                                >> Network.insertFetchedEdge Incoming id nid
                        )
                        model.network

            nw2 =
                outOnlyIds
                    |> List.foldl
                        (\nid ->
                            Network.updateAggEdge
                                (AggEdge.initId id nid)
                                (AggEdge.setLoading Outgoing id)
                                >> Network.insertFetchedEdge Outgoing id nid
                        )
                        nw
        in
        ( CheckingNeighbors.initAddress data outOnlyIds incOnlyIds model.checkingNeighbors
            |> flip s_checkingNeighbors model
            |> s_network nw2
        , getRelations id Outgoing autoLinkInTraceMode outOnlyIds
            ++ getRelations id Incoming autoLinkInTraceMode incOnlyIds
        )


getRelations : Id -> Direction -> Bool -> List Id -> List Effect
getRelations id dir autoLinkInTraceMode onlyIds =
    if List.isEmpty onlyIds then
        []

    else
        BrowserGotRelationsToVisibleNeighbors { id = id, dir = dir, requestIds = onlyIds, autoLinkInTraceMode = autoLinkInTraceMode }
            |> Api.GetAddressNeighborsEffect
                { currency = Id.network id
                , address = Id.id id
                , isOutgoing = dir == Outgoing
                , onlyIds =
                    onlyIds
                        |> List.map Id.id
                        |> Just
                , includeLabels = False
                , includeActors = False
                , pagesize = List.length onlyIds
                , nextpage = Nothing
                }
            |> ApiEffect
            |> List.singleton


handleTx : Plugins -> Update.Config -> { direction : Direction, addressId : Id } -> Maybe Id -> Api.Data.Tx -> Model -> ( Model, List Effect )
handleTx plugins uc config neighborId tx model =
    case neighborId of
        Just nid ->
            let
                newModel =
                    CheckingNeighbors.remove nid config.addressId model.checkingNeighbors
                        |> flip s_checkingNeighbors model
            in
            if GTx.hasAddress config.direction (Id.id nid) tx then
                addTx plugins uc config.addressId config.direction (Just nid) tx newModel

            else
                placeNeighborIfError plugins uc config nid model

        Nothing ->
            addTx plugins uc config.addressId config.direction Nothing tx model


placeNeighborIfError : Plugins -> Update.Config -> { direction : Direction, addressId : Id } -> Id -> Model -> ( Model, List Effect )
placeNeighborIfError plugins uc config nid model =
    let
        newModel =
            CheckingNeighbors.remove nid config.addressId model.checkingNeighbors
                |> flip s_checkingNeighbors model
    in
    if CheckingNeighbors.isEmpty nid newModel.checkingNeighbors then
        CheckingNeighbors.getData nid model.checkingNeighbors
            |> Maybe.map
                (\data ->
                    browserGotAddressData uc plugins nid (NextTo ( config.direction, config.addressId )) data newModel
                        |> mapSecond
                            ((::)
                                (NoAdjacentTxForAddressAndNeighborFound config.addressId nid
                                    |> InfoError
                                    |> ErrorEffect
                                )
                            )
                )
            |> Maybe.withDefault (n newModel)

    else
        n newModel


handleWorkflowNextUtxo : Plugins -> Update.Config -> WorkflowNextUtxoTx.Config -> Maybe Id -> WorkflowNextUtxoTx.Workflow -> Model -> ( Model, List Effect )
handleWorkflowNextUtxo plugins uc config neighborId wf model =
    case wf of
        Workflow.Ok tx ->
            Api.Data.TxTxUtxo tx
                |> flip (handleTx plugins uc config neighborId) model

        Workflow.Next eff ->
            eff
                |> List.map (Api.map (WorkflowNextUtxoTx config neighborId))
                |> List.map ApiEffect
                |> pair model

        Workflow.Err err ->
            case neighborId of
                Just nid ->
                    placeNeighborIfError plugins uc config nid model

                Nothing ->
                    case err of
                        WorkflowNextUtxoTx.NoTxFound ->
                            ( model
                                |> s_network (Network.updateAddress config.addressId (Txs Set.empty |> txsSetter config.direction) model.network)
                            , NoAdjaccentTxForAddressFound config.addressId
                                |> InfoError
                                |> ErrorEffect
                                |> List.singleton
                            )

                        WorkflowNextUtxoTx.MaxChangeHopsLimit maxHops lastTx ->
                            ( model
                                |> s_network
                                    (Network.updateAddress config.addressId (TxsLastCheckedChangeTx lastTx |> txsSetter config.direction) model.network)
                            , MaxChangeHopsLimitReached maxHops config.addressId
                                |> InfoError
                                |> ErrorEffect
                                |> List.singleton
                            )


handleWorkflowNextTxByTime : Plugins -> Update.Config -> WorkflowNextTxByTime.Config -> Maybe Id -> WorkflowNextTxByTime.Workflow -> Model -> ( Model, List Effect )
handleWorkflowNextTxByTime plugins uc config neighborId wf model =
    case wf of
        Workflow.Ok tx ->
            handleTx plugins uc config neighborId tx model

        Workflow.Next eff ->
            eff
                |> List.map (Api.map (WorkflowNextTxByTime config neighborId))
                |> List.map ApiEffect
                |> pair model

        Workflow.Err WorkflowNextTxByTime.NoTxFound ->
            case neighborId of
                Just nid ->
                    placeNeighborIfError plugins uc config nid model

                Nothing ->
                    ( model
                        |> s_network (Network.updateAddress config.addressId (Txs Set.empty |> txsSetter config.direction) model.network)
                    , NoAdjaccentTxForAddressFound config.addressId
                        |> InfoError
                        |> ErrorEffect
                        |> List.singleton
                    )


browserGotAddressData : Update.Config -> Plugins -> Id -> FindPosition -> Api.Data.Address -> Model -> ( Model, List Effect )
browserGotAddressData uc plugins providedId position data model =
    let
        id =
            providedId |> Tuple.mapSecond (Data.normalizeIdentifier (Id.network providedId))

        ( newAddress, net ) =
            Network.addAddressWithPosition plugins model.config position id model.network
                |> mapSecond (Network.updateAddress id (s_data (Success data)))

        clusterId =
            Id.initClusterId data.currency data.entity

        isSecondAddressFromSameCluster =
            Network.isClusterFriendAlreadyOnGraph clusterId
                model.network
                && not (Network.hasLoadedAddress id model.network)

        ncolors =
            if isSecondAddressFromSameCluster then
                Colors.assignNextColor Colors.Clusters clusterId model.colors

            else
                model.colors

        ( clusters, effCluster ) =
            if Dict.member clusterId model.clusters || Data.isAccountLike data.currency then
                ( model.clusters, [] )

            else
                ( Dict.insert clusterId RemoteData.Loading model.clusters
                , [ BrowserGotClusterData id
                        |> Api.GetEntityEffectWithDetails
                            { currency = Id.network id
                            , entity = data.entity
                            , includeActors = False
                            , includeBestTag = False
                            }
                        |> ApiEffect
                  ]
                )

        transform =
            (uc.size
                |> Maybe.map
                    (\{ width, height } ->
                        { width = width
                        , height = height
                        }
                    )
                |> Maybe.map Transform.politeMove
                |> Maybe.withDefault Transform.move
            )
                { x = newAddress.x * unit
                , y = A.getTo newAddress.y * unit
                , z = Transform.initZ
                }
                model.transform
    in
    model
        |> s_network net
        |> s_transform transform
        |> updateTagDataOnAddress id
        --|> s_details details
        |> s_colors ncolors
        |> s_clusters clusters
        |> pairTo
            (fetchTagSummaryForId True model.tagSummaries id
                :: fetchActorsForAddress data model.actors
                --++ eff
                ++ effCluster
                ++ [ InternalEffect (InternalPathfinderAddedAddress newAddress.id) ]
            )
        |> and (checkSelection uc)


handleTooltipMsg : AddressDetails.TooltipMsgs -> Model -> ( Model, List Effect )
handleTooltipMsg msg model =
    case msg of
        AddressDetails.RelatedAddressesTooltipMsg inner ->
            case inner of
                AddressDetails.ShowRelatedAddressesTooltip config ->
                    ( model, OpenTooltipEffect { context = config.domId, domId = config.domId } False (Tooltip.Text config.text) |> List.singleton )

                AddressDetails.HideRelatedAddressesTooltip config ->
                    ( model, CloseTooltipEffect (Just { context = config.domId, domId = config.domId }) True |> List.singleton )

        AddressDetails.TagTooltipMsg inner ->
            case inner of
                Tag.UserMovesMouseOutTagConcept ctx ->
                    ( model, CloseTooltipEffect (Just ctx) True |> List.singleton )

                Tag.UserMovesMouseOverTagConcept ctx ->
                    let
                        decoder =
                            Json.Decode.map3 Tuple3.join
                                (Json.Decode.index 0 Json.Decode.string)
                                (Json.Decode.index 1 Json.Decode.string)
                                (Json.Decode.index 2 Json.Decode.string)
                    in
                    Json.Decode.decodeString decoder ctx.context
                        |> Result.map
                            (\( concept, currency, address ) ->
                                let
                                    id =
                                        Id.init currency address

                                    tsToTooltip ts =
                                        Tooltip.TagConcept id
                                            concept
                                            ts
                                            { openTooltip =
                                                Tag.UserMovesMouseOverTagConcept ctx
                                                    |> AddressDetails.TagTooltipMsg
                                                    |> AddressDetails.TooltipMsg
                                                    |> AddressDetailsMsg id
                                            , closeTooltip =
                                                Tag.UserMovesMouseOutTagConcept ctx
                                                    |> AddressDetails.TagTooltipMsg
                                                    |> AddressDetails.TooltipMsg
                                                    |> AddressDetailsMsg id
                                            , openDetails = Just (UserOpensDialogWindow (TagsList id))
                                            }
                                in
                                case Dict.get id model.tagSummaries of
                                    Just (HasTagSummaries { withCluster }) ->
                                        ( model
                                        , OpenTooltipEffect ctx False (tsToTooltip withCluster) |> List.singleton
                                        )

                                    Just (HasTagSummaryWithCluster ts) ->
                                        ( model
                                        , OpenTooltipEffect ctx False (tsToTooltip ts) |> List.singleton
                                        )

                                    Just (HasTagSummaryOnlyWithCluster ts) ->
                                        ( model
                                        , OpenTooltipEffect ctx False (tsToTooltip ts) |> List.singleton
                                        )

                                    _ ->
                                        n model
                            )
                        |> Result.withDefault (n model)


userClickedAddressCheckboxInTable : Plugins -> Id -> Model -> ( Model, List Effect )
userClickedAddressCheckboxInTable plugins id model =
    if Dict.member id model.network.addresses then
        removeAddress id model

    else
        loadAddress plugins True id model


userClickedAggEdgeCheckboxInTable : Plugins -> Direction -> Id -> Api.Data.NeighborAddress -> Model -> ( Model, List Effect )
userClickedAggEdgeCheckboxInTable plugins dir anchorId data model =
    let
        id =
            Id.init data.address.currency data.address.address

        flippedDir =
            Direction.flip dir

        aggEdgeId =
            AggEdge.initId id anchorId
    in
    if Network.hasAddress id model.network then
        if Network.hasAggEdge aggEdgeId model.network then
            removeAggEdge aggEdgeId model
                |> and
                    (\newModel ->
                        if Dict.member id newModel.network.addressAggEdgeMap then
                            n newModel

                        else
                            removeAddress id newModel
                    )

        else
            ( model.network
                |> Network.upsertAggEdgeData model.config anchorId dir data
                |> flip s_network model
            , BrowserGotRelationsToVisibleNeighbors { id = anchorId, dir = flippedDir, requestIds = [ id ], autoLinkInTraceMode = True }
                |> Api.GetAddressNeighborsEffect
                    { currency = Id.network anchorId
                    , address = Id.id anchorId
                    , isOutgoing = flippedDir == Outgoing
                    , onlyIds = Just [ data.address.address ]
                    , includeLabels = False
                    , includeActors = False
                    , pagesize = 1
                    , nextpage = Nothing
                    }
                |> ApiEffect
                |> List.singleton
            )

    else
        loadAddressWithPosition plugins True (NextTo ( dir, anchorId )) id model


userClickedTx : Id -> Model -> ( Model, List Effect )
userClickedTx id model =
    if model.modPressed then
        let
            modelS =
                multiSelect model [ MSelectedTx id ] True
        in
        n { modelS | details = Nothing }

    else
        ( model
        , Route.txRoute
            { network = Id.network id
            , txHash = Id.id id
            }
            |> NavPushRouteEffect
            |> List.singleton
        )


fitGraph : Update.Config -> Model -> Model
fitGraph uc model =
    { model
        | transform =
            uc.size
                |> Maybe.map
                    (\{ width, height } ->
                        { width = width - searchBoxMinWidth / 2
                        , height = height
                        }
                    )
                |> Maybe.map
                    (\viewport ->
                        Network.getBoundingBox model.network
                            |> addMarginPathfinder
                            |> flip (Transform.updateByBoundingBox viewport) model.transform
                    )
                |> Maybe.withDefault model.transform
    }


expandAddress : Address -> Direction -> Model -> ( Model, List Effect )
expandAddress address direction model =
    let
        id =
            address.id

        ( newmodel, eff ) =
            model
                |> selectAddress id

        setter =
            txsSetter direction

        setLoading =
            s_network
                (Network.updateAddress id (setter TxsLoading) newmodel.network)
    in
    case getTxs address direction of
        Txs _ ->
            n newmodel

        TxsLoading ->
            n newmodel

        TxsLastCheckedChangeTx tx ->
            ( newmodel
                |> setLoading
            , let
                config =
                    { addressId = id
                    , direction = direction
                    }
              in
              WorkflowNextUtxoTx.start config tx
                |> Workflow.mapEffect (WorkflowNextUtxoTx config Nothing)
                |> Workflow.next
                |> List.map ApiEffect
            )

        TxsNotFetched ->
            ( newmodel |> setLoading
            , Nothing
                |> getNextTxEffects newmodel.network id direction { addBetweenLinks = False, addAnyLinks = True }
                |> (++) eff
            )


deleteSelection : Model -> ( Model, List Effect )
deleteSelection model =
    (case model.selection of
        SelectedConversionEdge ( _, _ ) ->
            n model

        SelectedAddress id ->
            removeAddress id model

        SelectedTx id ->
            removeTx id model

        MultiSelect items ->
            List.foldl
                (\i ( m, _ ) ->
                    case i of
                        MSelectedAddress id ->
                            removeAddress id m

                        MSelectedTx id ->
                            removeTx id m
                )
                ( model, [] )
                items

        SelectedAggEdge id ->
            removeAggEdge id model

        WillSelectTx _ ->
            n model

        WillSelectAddress _ ->
            n model

        WillSelectAggEdge _ ->
            n model

        NoSelection ->
            n model
    )
        |> and unselect


updateTagDataOnAddress : Id -> Model -> Model
updateTagDataOnAddress addressId m =
    let
        tag =
            Dict.get addressId m.tagSummaries

        updateTagsummaryData tagdata =
            let
                actorlabel =
                    case tagdata.bestActor of
                        Just actor ->
                            Dict.get actor m.actors
                                |> Maybe.map .label
                                |> Maybe.Extra.orElse tagdata.bestLabel

                        _ ->
                            Nothing
            in
            (if TagSummary.isExchangeNode tagdata then
                Network.updateAddress addressId
                    (s_exchange tagdata.bestLabel)
                    m.network

             else
                m.network
            )
                |> Network.updateAddress addressId (s_hasTags (tagdata.tagCount > 0 && not (TagSummary.hasOnlyExchangeTags tagdata)))
                |> Network.updateAddress addressId (s_actor actorlabel)

        net td =
            case td of
                HasTagSummaries { withCluster } ->
                    updateTagsummaryData withCluster

                HasTagSummaryWithCluster ts ->
                    updateTagsummaryData ts

                HasTagSummaryOnlyWithCluster ts ->
                    updateTagsummaryData ts

                HasExchangeTagOnly ->
                    Network.updateAddress addressId (s_hasTags False) m.network

                HasTags _ ->
                    Network.updateAddress addressId (s_hasTags True) m.network

                _ ->
                    m.network
    in
    tag |> Maybe.map (\n -> { m | network = net n }) |> Maybe.withDefault m


getNextTxEffects : Network -> Id -> Direction -> { addBetweenLinks : Bool, addAnyLinks : Bool } -> Maybe Id -> List Effect
getNextTxEffects network addressId direction { addBetweenLinks, addAnyLinks } neighborId =
    let
        config =
            { addressId = addressId
            , direction = direction
            }
    in
    Network.getRecentTxForAddress network (Direction.flip direction) addressId
        |> Maybe.map
            (\tx ->
                if addAnyLinks then
                    case tx.type_ of
                        Tx.Account t ->
                            WorkflowNextTxByTime.startByHeight config t.raw.height t.raw.currency
                                |> Workflow.mapEffect (WorkflowNextTxByTime config neighborId)
                                |> Workflow.next
                                |> List.map ApiEffect

                        Tx.Utxo t ->
                            WorkflowNextUtxoTx.start config t.raw
                                |> Workflow.mapEffect (WorkflowNextUtxoTx config neighborId)
                                |> Workflow.next
                                |> List.map ApiEffect

                else
                    []
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                if addBetweenLinks then
                    neighborId
                        |> Maybe.map (WorkflowNextTxByTime.startBetween config)
                        |> Maybe.withDefault (WorkflowNextTxByTime.start config)
                        |> Workflow.mapEffect (WorkflowNextTxByTime config neighborId)
                        |> Workflow.next
                        |> List.map ApiEffect

                else if addAnyLinks then
                    WorkflowNextTxByTime.start config
                        |> Workflow.mapEffect (WorkflowNextTxByTime config neighborId)
                        |> Workflow.next
                        |> List.map ApiEffect

                else
                    []
            )


updateByRoute : Plugins -> Update.Config -> Route -> Model -> ( Model, List Effect )
updateByRoute plugins uc route model =
    let
        pathfinderReady =
            uc.size /= Nothing
    in
    if not pathfinderReady then
        ( model
        , [ PostponeUpdateByRouteEffect route
          ]
        )

    else
        forcePushHistory (model |> s_isDirty True |> s_route route)
            |> updateByRoute_ plugins uc route
            |> and (syncSidePanel uc)


addPathsToGraph : Plugins -> Update.Config -> Model -> String -> { x | outgoing : Bool } -> List (List PathHopType) -> ( Model, List Effect )
addPathsToGraph plugins uc model net config listOfPaths =
    let
        baseModelUnselected =
            unselect model
    in
    List.foldl
        (\paths ( m, eff ) ->
            addPathToGraph plugins uc m net config paths
                |> Tuple.mapSecond ((++) eff)
        )
        baseModelUnselected
        listOfPaths


addPathToGraph : Plugins -> Update.Config -> Model -> String -> { x | outgoing : Bool } -> List PathHopType -> ( Model, List Effect )
addPathToGraph plugins uc model net config list =
    let
        getAddress adr =
            case adr of
                Route.AddressHop _ a ->
                    Just (Data.normalizeIdentifier net a)

                _ ->
                    Nothing

        pathTypeToAddressId pt =
            case pt of
                AddressHop _ x ->
                    Just (Id.init net (Data.normalizeIdentifier net x))

                _ ->
                    Nothing

        pathTypeToSelection pt =
            case pt of
                AddressHop _ x ->
                    MSelectedAddress (Id.init net (Data.normalizeIdentifier net x))

                TxHop txh ->
                    MSelectedTx (Id.init net txh)

        startAddressCoords =
            list
                |> List.head
                |> Maybe.andThen pathTypeToAddressId
                |> Maybe.andThen (flip Network.getAddressCoords model.network)

        newSelections =
            list |> List.map pathTypeToSelection

        startCoordsPrel =
            startAddressCoords
                |> Maybe.withDefault { x = 0, y = 0 }

        startY =
            Network.getYForPathAfterX model.network startCoordsPrel.x startCoordsPrel.y

        startCoords =
            startCoordsPrel |> s_y (max startY startCoordsPrel.y)

        startAddressOnGraphAlready =
            startAddressCoords /= Nothing

        isDuplicateAddress i =
            Maybe.andThen
                (\adr ->
                    list
                        |> List.take i
                        |> List.Extra.find (getAddress >> (==) (Just adr))
                )
                >> (/=) Nothing

        addressCount =
            list
                |> List.filterMap getAddress
                |> List.foldl
                    (\a -> Dict.update a (Maybe.map ((+) 1) >> Maybe.withDefault 0 >> Just))
                    Dict.empty

        accf ( i, a ) { m, eff, x, y, previousAddress } =
            let
                address =
                    getAddress a

                prevCount =
                    previousAddress
                        |> Maybe.andThen (flip Dict.get addressCount)
                        |> Maybe.withDefault 0

                ( xOffset, yOffset ) =
                    if isDuplicateAddress (i - 1) previousAddress && prevCount > 0 then
                        ( 0, 0 )

                    else if isDuplicateAddress i address then
                        ( 0, 0 )

                    else
                        ( if startAddressOnGraphAlready && i == 0 then
                            0

                          else
                            nodeXOffset
                                * (if config.outgoing then
                                    1

                                   else
                                    -1
                                  )
                        , 0
                        )

                x_ =
                    x + xOffset

                y_ =
                    y + yOffset

                action =
                    case a of
                        Route.AddressHop _ adr ->
                            loadAddressWithPosition plugins True (Fixed x_ y_) ( net, Data.normalizeIdentifier net adr )

                        Route.TxHop h ->
                            loadTxWithPosition (Fixed x_ y_) True False plugins ( net, h )

                annotations =
                    case a of
                        Route.AddressHop VictimAddress adr ->
                            Annotations.set
                                ( net, Data.normalizeIdentifier net adr )
                                (Locale.string uc.locale "victim")
                                (Just annotationGreen)
                                m.annotations

                        Route.AddressHop PerpetratorAddress adr ->
                            Annotations.set
                                ( net, Data.normalizeIdentifier net adr )
                                (Locale.string uc.locale "perpetrator")
                                (Just annotationRed)
                                m.annotations

                        _ ->
                            m.annotations

                ( nm, effn ) =
                    m |> s_annotations annotations |> action
            in
            { m = nm
            , eff = eff ++ effn
            , x = x_
            , y = y_
            , previousAddress = address
            }

        result =
            list
                |> List.indexedMap pair
                |> List.foldl accf
                    { m = model
                    , eff = []
                    , x = startCoords.x
                    , y = startCoords.y
                    , previousAddress = Nothing
                    }
    in
    ( result.m |> (\m -> multiSelect m newSelections True) |> fitGraph uc, result.eff )


updateByRoute_ : Plugins -> Update.Config -> Route -> Model -> ( Model, List Effect )
updateByRoute_ plugins uc route model =
    case route |> Log.log "route" of
        Route.Root ->
            unselect model

        Route.Network network (Route.Address a _) ->
            let
                id =
                    Id.init network a
            in
            { model | network = Network.clearSelection model.network }
                |> loadAddress plugins True id
                |> and (selectAddress id)

        Route.Network network (Route.Tx a) ->
            let
                id =
                    Id.init network a
            in
            { model | network = Network.clearSelection model.network }
                |> loadTx True True plugins id
                |> and (selectTx id)

        Route.Network network (Route.Relation a b) ->
            let
                aId =
                    Id.init network a

                bId =
                    Id.init network b
            in
            { model | network = Network.clearSelection model.network }
                |> loadAddress plugins True aId
                |> and (loadAddress plugins True bId)
                |> and (selectAggEdge uc (AggEdge.initId aId bId))
                |> and (setTracingMode AggregateTracingMode)

        Route.Path net list ->
            addPathToGraph plugins uc model net { outgoing = True } list

        _ ->
            n model


updateByPluginOutMsg : Plugins -> Update.Config -> List Plugin.OutMsg -> Model -> ( Model, List Effect )
updateByPluginOutMsg plugins uc outMsgs model =
    outMsgs
        |> List.foldl
            (\msg ( mo, eff ) ->
                case Log.log "outMsgPF" msg of
                    PluginInterface.ShowBrowser ->
                        ( mo, eff )

                    PluginInterface.OutMsgsPathfinder (PluginInterface.ShowPathsInPathfinder net paths) ->
                        addPathsToGraph plugins uc mo net { outgoing = True } paths
                            |> Tuple.mapSecond ((++) eff)

                    PluginInterface.OutMsgsPathfinder (PluginInterface.ShowPathsInPathfinderWithConfig net c paths) ->
                        addPathsToGraph plugins uc mo net c paths
                            |> Tuple.mapSecond ((++) eff)

                    PluginInterface.UpdateAddresses { currency, address } pmsg ->
                        let
                            pId =
                                ( currency, address )
                        in
                        ( { mo
                            | network = Network.updateAddress pId (Plugin.updateAddress plugins pmsg) mo.network
                          }
                        , eff
                        )

                    PluginInterface.UpdateAddressesByRootAddress { currency, address } pmsg ->
                        model.clusters
                            |> Dict.values
                            |> List.filterMap RemoteData.toMaybe
                            |> List.Extra.find
                                (\e ->
                                    e.currency == currency && e.rootAddress == address
                                )
                            |> Maybe.map Id.initClusterIdFromRecord
                            |> Maybe.map
                                (\pId ->
                                    ( { mo
                                        | network = Network.updateAddressesByClusterId pId (Plugin.updateAddress plugins pmsg) mo.network
                                      }
                                    , eff
                                    )
                                )
                            |> Maybe.withDefault ( mo, eff )

                    PluginInterface.UpdateAddressesByEntityPathfinder e pmsg ->
                        let
                            pId =
                                Id.initClusterIdFromRecord e
                        in
                        ( { mo
                            | network = Network.updateAddressesByClusterId pId (Plugin.updateAddress plugins pmsg) mo.network
                          }
                        , eff
                        )

                    PluginInterface.UpdateAddressEntities _ _ ->
                        ( mo, eff )

                    PluginInterface.UpdateEntities _ _ ->
                        ( mo, eff )

                    PluginInterface.UpdateEntitiesByRootAddress _ _ ->
                        ( mo, eff )

                    PluginInterface.LoadAddressIntoGraph _ ->
                        ( mo, eff )

                    PluginInterface.GetEntitiesForAddresses _ _ ->
                        ( mo, eff )

                    PluginInterface.GetEntities _ _ ->
                        ( mo, eff )

                    PluginInterface.PushUrl _ ->
                        ( mo, eff )

                    PluginInterface.GetSerialized _ ->
                        ( mo, eff )

                    PluginInterface.Deserialize _ _ ->
                        ( mo, eff )

                    PluginInterface.GetAddressDomElement _ _ ->
                        ( mo, eff )

                    PluginInterface.SendToPort _ ->
                        ( mo, eff )

                    PluginInterface.ApiRequest _ ->
                        ( mo, eff )

                    PluginInterface.ShowDialog _ ->
                        ( mo, eff )

                    PluginInterface.CloseDialog ->
                        ( mo, eff )

                    PluginInterface.ShowNotification _ ->
                        ( mo, eff )

                    PluginInterface.OutMsgsPathfinder _ ->
                        ( mo, eff )

                    PluginInterface.OpenTooltip s msgs ->
                        ( mo, [ OpenTooltipEffect s False (Tooltip.Plugin s (Tooltip.mapMsgTooltipMsg msgs PluginMsg)) ] )

                    PluginInterface.CloseTooltip s withDelay ->
                        ( mo, [ CloseTooltipEffect (Just s) withDelay ] )
            )
            ( model, [] )


loadAddress : Plugins -> Bool -> Id -> Model -> ( Model, List Effect )
loadAddress plugins autoLinkTxInTraceMode =
    loadAddressWithPosition plugins autoLinkTxInTraceMode Auto


loadAddressWithPosition : Plugins -> Bool -> FindPosition -> Id -> Model -> ( Model, List Effect )
loadAddressWithPosition _ autoLinkTxInTraceMode position id model =
    let
        request =
            ( -- don't add the address here because it is not loaded yet
              --{ model | network = Network.addAddressWithPosition plugins position id model.network }
              model
            , [ BrowserGotAddressData
                    { id = id
                    , pos = position
                    , autoLinkTxInTraceMode = autoLinkTxInTraceMode
                    }
                    |> Api.GetAddressEffect
                        { currency = Id.network id
                        , address = Id.id id
                        , includeActors = True
                        }
                    |> ApiEffect
              ]
            )
    in
    Dict.get id model.network.addresses
        |> Maybe.map
            (\address ->
                case address.data of
                    RemoteData.Success _ ->
                        n model

                    RemoteData.Loading ->
                        n model

                    _ ->
                        request
            )
        |> Maybe.withDefault request


loadTxWithPosition : FindPosition -> Bool -> Bool -> Plugins -> Id -> Model -> ( Model, List Effect )
loadTxWithPosition pos autoLinkInTraceMode loadAddresses _ id model =
    ( model
    , BrowserGotTx { pos = pos, loadAddresses = loadAddresses, autoLinkInTraceMode = autoLinkInTraceMode }
        |> Api.GetTxEffect
            { currency = Id.network id
            , txHash = Id.id id
            , includeIo = True
            , tokenTxId = Nothing
            }
        |> ApiEffect
        |> List.singleton
    )


loadTx : Bool -> Bool -> Plugins -> Id -> Model -> ( Model, List Effect )
loadTx =
    loadTxWithPosition Auto


selectTx : Id -> Model -> ( Model, List Effect )
selectTx id model =
    case Dict.get id model.network.txs of
        Just tx ->
            let
                ( m1, eff ) =
                    unselect model
            in
            Network.updateTx id (s_selected True) m1.network
                |> flip s_network m1
                |> s_selection (SelectedTx id)
                |> bulkfetchTagsForTx tx
                |> Tuple.mapSecond ((++) eff)

        Nothing ->
            s_selection (WillSelectTx id) model
                |> n


selectAggEdge : Update.Config -> ( Id, Id ) -> Model -> ( Model, List Effect )
selectAggEdge _ id model =
    case Dict.get id model.network.aggEdges of
        Just _ ->
            let
                ( m1, eff ) =
                    unselect model
            in
            Network.updateAggEdge id (s_selected True) m1.network
                |> flip s_network m1
                |> s_selection (SelectedAggEdge id)
                |> n
                |> Tuple.mapSecond ((++) eff)

        Nothing ->
            s_selection (WillSelectAggEdge id) model
                |> n


selectConversionEdge : ( Id, Id ) -> Model -> ( Model, List Effect )
selectConversionEdge ( a, b ) model =
    let
        ( m1, eff ) =
            unselect model
    in
    Network.updateConversionEdge ( a, b ) (s_selected True) m1.network
        |> Network.updateTx a (s_hovered True)
        |> Network.updateTx b (s_hovered True)
        |> flip s_network m1
        |> s_selection (SelectedConversionEdge ( a, b ))
        |> n
        |> Tuple.mapSecond ((++) eff)


selectAddress : Id -> Model -> ( Model, List Effect )
selectAddress id model =
    case Dict.get id model.network.addresses of
        Just _ ->
            let
                ( m1, eff2 ) =
                    unselect model
            in
            Network.updateAddress id (s_selected True) m1.network
                |> flip s_network m1
                |> s_selection (SelectedAddress id)
                |> pairTo eff2

        Nothing ->
            s_selection (WillSelectAddress id) model
                |> n


unselect : Model -> ( Model, List Effect )
unselect model =
    let
        unselectAddress a nw =
            Network.updateAddress a (s_selected False) nw

        unselectTx a nw =
            Network.updateTx a (s_selected False) nw

        unselectAggEdge a nw =
            Network.updateAggEdge a (s_selected False) nw

        unselectConversionEdge ( a, b ) nw =
            Network.updateConversionEdge ( a, b ) (s_selected False >> s_hovered False) nw
                |> Network.updateTx a (s_hovered False)
                |> Network.updateTx b (s_hovered False)

        network =
            case model.selection of
                SelectedConversionEdge id ->
                    unselectConversionEdge id model.network

                SelectedAddress a ->
                    unselectAddress a model.network

                SelectedTx a ->
                    unselectTx a model.network

                SelectedAggEdge a ->
                    unselectAggEdge a model.network

                MultiSelect aa ->
                    aa
                        |> List.foldl
                            (\m nw ->
                                case m of
                                    MSelectedAddress a ->
                                        unselectAddress a nw

                                    MSelectedTx a ->
                                        unselectTx a nw
                            )
                            model.network

                WillSelectTx _ ->
                    model.network

                WillSelectAddress _ ->
                    model.network

                WillSelectAggEdge _ ->
                    model.network

                NoSelection ->
                    model.network
    in
    ( network
        |> flip s_network model
        |> s_details Nothing
        |> s_selection NoSelection
        |> s_modPressed False
    , [ CloseTooltipEffect Nothing False ]
    )


unhover : Model -> Model
unhover model =
    let
        network =
            case model.hovered of
                HoveredAddress _ ->
                    model.network

                --Network.updateAddress a (s_hovered False) model.network
                HoveredTx a ->
                    Network.updateTx a (s_hovered False) model.network
                        |> Network.trySetHoverConversionLoop a False

                HoveredAggEdge a ->
                    Network.updateAggEdge a (s_hovered False) model.network

                HoveredConversionEdge ( a, b ) ->
                    Network.updateConversionEdge ( a, b ) (s_hovered False) model.network

                NoHover ->
                    model.network
    in
    network
        |> flip s_network model
        |> s_hovered NoHover


pushHistory : Plugins -> Msg -> Model -> Model
pushHistory plugins msg model =
    if History.shallPushHistory plugins msg model then
        forcePushHistory model

    else
        model


forcePushHistory : Model -> Model
forcePushHistory model =
    { model
        | history =
            makeHistoryEntry model
                |> History.push model.history
    }


makeHistoryEntry : Model -> Entry.Model
makeHistoryEntry model =
    { network = (unselect model |> Tuple.first |> unhover).network
    , annotations = model.annotations
    }


undoRedo : (History.Model Entry.Model -> Entry.Model -> Maybe ( History.Model Entry.Model, Entry.Model )) -> Model -> ( Model, List Effect )
undoRedo fun model =
    makeHistoryEntry model
        |> fun model.history
        |> Maybe.map
            (\( history, entry ) ->
                { model
                    | history = history
                    , network = entry.network
                    , selection = NoSelection
                    , annotations = entry.annotations
                }
            )
        |> Maybe.withDefault model
        |> n


markDirty : Plugins -> Msg -> Model -> Model
markDirty plugins msg model =
    if History.shallPushHistory plugins msg model then
        model |> s_isDirty True

    else
        model


fetchActor : String -> Effect
fetchActor id =
    BrowserGotActor id |> Api.GetActorEffect { actorId = id } |> ApiEffect


bulkfetchTagsForTx : Tx -> Model -> ( Model, List Effect )
bulkfetchTagsForTx tx model =
    case tx.type_ of
        Tx.Utxo { raw } ->
            let
                addresses x =
                    x
                        |> Maybe.map (List.concatMap .address)
                        |> Maybe.withDefault []
                        |> List.map (Id.init raw.currency)
                        |> List.filter (flip Dict.member model.tagSummaries >> not)
            in
            addresses raw.inputs
                ++ addresses raw.outputs
                |> Set.fromList
                |> Set.toList
                |> bulkfetchTagsForAddresses raw.currency model

        _ ->
            n model


bulkfetchTagsForAddresses : String -> Model -> List Id -> ( Model, List Effect )
bulkfetchTagsForAddresses network model addr =
    ( { model
        | tagSummaries =
            addr
                |> List.foldl
                    -- (\a -> Dict.insert a LoadingTags)
                    (\id -> upsertTagSummary id LoadingTags)
                    model.tagSummaries
      }
    , List.Extra.greedyGroupsOf 100 addr
        |> List.map
            (\adr ->
                BrowserGotAddressesTags adr
                    |> Api.BulkGetAddressTagsEffect
                        { currency = network
                        , addresses = List.map Id.id adr
                        , pagesize = Just 1
                        , includeBestClusterTag = True
                        }
                    |> ApiEffect
            )
    )


isTagSummaryLoaded : Bool -> Dict Id HavingTags -> Id -> Bool
isTagSummaryLoaded includeBestClusterTag existing id =
    case Dict.get id existing of
        Just (HasTagSummaries _) ->
            True

        Just (HasTagSummaryOnlyWithCluster _) ->
            True

        Just (HasTagSummaryWithCluster _) ->
            includeBestClusterTag

        Just (HasTagSummaryWithoutCluster _) ->
            includeBestClusterTag == False

        Just NoTagsWithoutCluster ->
            includeBestClusterTag == False

        _ ->
            False


fetchTagSummaryForIds : Bool -> Dict Id HavingTags -> List Id -> Effect
fetchTagSummaryForIds includeBestClusterTag existing ids =
    let
        idsToLoad =
            ids |> List.filter (isTagSummaryLoaded includeBestClusterTag existing >> not)
    in
    case idsToLoad of
        [] ->
            CmdEffect Cmd.none

        x :: _ ->
            BrowserGotTagSummaries includeBestClusterTag
                |> Api.BulkGetAddressTagSummaryEffect { currency = Id.network x, addresses = idsToLoad |> List.map Id.id, includeBestClusterTag = includeBestClusterTag }
                |> ApiEffect


fetchTagSummaryForId : Bool -> Dict Id HavingTags -> Id -> Effect
fetchTagSummaryForId includeBestClusterTag existing id =
    let
        fetch =
            BrowserGotTagSummary includeBestClusterTag id
                |> Api.GetAddressTagSummaryEffect { currency = Id.network id, address = Id.id id, includeBestClusterTag = includeBestClusterTag }
                |> ApiEffect
    in
    if isTagSummaryLoaded includeBestClusterTag existing id then
        CmdEffect Cmd.none

    else
        fetch


addTagSummaryToModel : ( Model, List Effect ) -> Bool -> Id -> Api.Data.TagSummary -> ( Model, List Effect )
addTagSummaryToModel ( m, e ) includesBestClusterTag id data =
    let
        d =
            if data.tagCount > 0 && includesBestClusterTag then
                HasTagSummaryWithCluster data

            else if data.tagCount > 0 && not includesBestClusterTag then
                HasTagSummaryWithoutCluster data

            else if data.tagCount == 0 && not includesBestClusterTag then
                NoTagsWithoutCluster

            else
                NoTags
    in
    ( { m
        | tagSummaries = upsertTagSummary id d m.tagSummaries
      }
        |> updateTagDataOnAddress id
    , e ++ (data.bestActor |> Maybe.map (List.singleton >> flip fetchActors m.actors) |> Maybe.withDefault [])
    )


fetchActorsForAddress : Api.Data.Address -> Dict String Api.Data.Actor -> List Effect
fetchActorsForAddress d existing =
    d.actors
        |> Maybe.map (List.filter (\l -> not (Dict.member l.id existing)))
        |> Maybe.map (List.map (.id >> fetchActor))
        |> Maybe.withDefault []


fetchActors : List String -> Dict String Api.Data.Actor -> List Effect
fetchActors d existing =
    d
        |> List.filter (\l -> not (Dict.member l existing))
        |> List.map fetchActor


getBiggestIO : Maybe (List Api.Data.TxValue) -> Set String -> Maybe String
getBiggestIO io exceptAddresses =
    io
        |> Maybe.withDefault []
        |> List.filter (\x -> x.address |> Set.fromList |> Set.intersect exceptAddresses |> Set.isEmpty)
        |> List.sortBy (.value >> .value)
        |> List.reverse
        |> List.head
        |> Maybe.map .address
        |> Maybe.andThen List.head


getAddressForDirection : Tx -> Direction -> Set String -> Maybe Id
getAddressForDirection tx direction exceptAddress =
    case tx.type_ of
        Tx.Utxo { raw } ->
            (case direction of
                Incoming ->
                    getBiggestIO raw.inputs exceptAddress

                Outgoing ->
                    getBiggestIO raw.outputs exceptAddress
            )
                |> Maybe.map (Id.init raw.currency)

        Tx.Account { raw } ->
            (case direction of
                Incoming ->
                    raw.fromAddress

                Outgoing ->
                    raw.toAddress
            )
                |> Id.init raw.network
                |> Just


addTx : Plugins -> Update.Config -> Id -> Direction -> Maybe Id -> Api.Data.Tx -> Model -> ( Model, List Effect )
addTx plugins uc anchorAddressId direction addressId tx model =
    if Dict.member (Tx.getTxId tx) model.network.txs then
        n model

    else
        let
            posNewTx =
                Network.NextTo ( direction, anchorAddressId )

            ( newTx, network ) =
                Network.addTxWithPosition model.config posNewTx tx model.network

            transform =
                (uc.size
                    |> Maybe.map
                        (\{ width, height } ->
                            { width = width
                            , height = height
                            }
                        )
                    |> Maybe.map Transform.politeMove
                    |> Maybe.withDefault Transform.move
                )
                    { x = newTx.x * unit
                    , y = A.getTo newTx.y * unit
                    , z = Transform.initZ
                    }
                    model.transform

            newmodel =
                { model
                    | network = network
                    , transform = transform
                }

            address =
                Id.id anchorAddressId

            -- TODO what if multisig?
            firstAddress =
                addressId
                    |> Maybe.Extra.orElse
                        (getAddressForDirection newTx direction (Set.singleton address))
        in
        firstAddress
            |> Maybe.map
                (\a ->
                    let
                        position =
                            NextTo ( direction, newTx.id )
                    in
                    loadAddressWithPosition plugins True position a newmodel
                )
            |> Maybe.withDefault (n newmodel)
            |> and (autoLoadConversions plugins newTx)


checkSelection : Update.Config -> Model -> ( Model, List Effect )
checkSelection uc model =
    case model.selection of
        WillSelectTx id ->
            selectTx id model

        WillSelectAddress id ->
            selectAddress id model

        MultiSelect selections ->
            -- not using selectAddress/tx here since they deselect other stuff
            -- do some other steps.
            selections
                |> List.foldl
                    (\msel ( m, eff ) ->
                        case msel of
                            MSelectedAddress id ->
                                ( m |> s_network (Network.updateTx id (s_selected True) m.network), eff )

                            MSelectedTx id ->
                                ( m |> s_network (Network.updateTx id (s_selected True) m.network), eff )
                    )
                    ( model, [] )

        WillSelectAggEdge id ->
            selectAggEdge uc id model

        SelectedAddress _ ->
            n model

        SelectedConversionEdge _ ->
            n model

        SelectedTx _ ->
            n model

        SelectedAggEdge _ ->
            n model

        NoSelection ->
            n model


removeAddress : Id -> Model -> ( Model, List Effect )
removeAddress id model =
    { model
        | network = Network.deleteAddress id model.network
        , details =
            case model.details of
                Just (AddressDetails addressId _) ->
                    if addressId == id then
                        Nothing

                    else
                        model.details

                _ ->
                    model.details
        , selection =
            case model.selection of
                SelectedAddress addressId ->
                    if addressId == id then
                        NoSelection

                    else
                        model.selection

                _ ->
                    model.selection
    }
        |> removeIsolatedTransactions


removeTx : Id -> Model -> ( Model, List Effect )
removeTx id model =
    ( { model
        | network = Network.deleteTx id model.network
        , selection =
            case model.selection of
                SelectedAddress addressId ->
                    if addressId == id then
                        NoSelection

                    else
                        model.selection

                _ ->
                    model.selection
      }
    , [ CloseTooltipEffect Nothing False ]
    )


removeAggEdge : ( Id, Id ) -> Model -> ( Model, List Effect )
removeAggEdge id model =
    ( { model
        | network = Network.deleteAggEdge id model.network
        , selection =
            case model.selection of
                SelectedAggEdge aggId ->
                    if aggId == id then
                        NoSelection

                    else
                        model.selection

                _ ->
                    model.selection
      }
    , [ CloseTooltipEffect Nothing False ]
    )


isIsolatedTx : Model -> Tx -> Bool
isIsolatedTx model tx =
    case tx.type_ of
        Tx.Utxo x ->
            let
                keys =
                    Dict.keys x.outputs ++ Dict.keys x.inputs
            in
            not (List.any (\y -> Dict.member y model.network.addresses) keys)

        Tx.Account x ->
            not (Dict.member x.from model.network.addresses && Dict.member x.to model.network.addresses)


removeIsolatedTransactions : Model -> ( Model, List Effect )
removeIsolatedTransactions model =
    let
        idsToRemove =
            Dict.keys (Dict.filter (\_ v -> isIsolatedTx model v) model.network.txs)
    in
    List.foldl (\i ( m, _ ) -> removeTx i m) ( model, [] ) idsToRemove


multiSelect : Model -> List MultiSelectOptions -> Bool -> Model
multiSelect m sel keepOld =
    let
        newSelection =
            case m.selection of
                MultiSelect x ->
                    if keepOld then
                        List.Extra.unique (x ++ sel)

                    else
                        List.Extra.unique sel

                SelectedAddress oid ->
                    List.Extra.unique (MSelectedAddress oid :: sel)

                SelectedTx oid ->
                    List.Extra.unique (MSelectedTx oid :: sel)

                _ ->
                    sel

        liftedNewSelection =
            case newSelection of
                x :: [] ->
                    case x of
                        MSelectedAddress id ->
                            SelectedAddress id

                        MSelectedTx id ->
                            SelectedTx id

                _ ->
                    MultiSelect newSelection

        selectItem s item n =
            case item of
                MSelectedAddress id ->
                    Network.updateAddress id (s_selected s) n

                MSelectedTx id ->
                    Network.updateTx id (s_selected s) n

        nNet =
            List.foldl (selectItem True) (Network.clearSelection m.network) newSelection
    in
    { m | selection = liftedNewSelection, network = nNet }


deserialize : Json.Decode.Value -> Result Json.Decode.Error Deserialized
deserialize =
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                if str /= "pathfinder" then
                    Json.Decode.fail "no pathfinder graph data"

                else
                    Json.Decode.index 1 Json.Decode.string
                        |> Json.Decode.andThen deserializeByVersion
            )
        |> Json.Decode.decodeValue


deserializeByVersion : String -> Json.Decode.Decoder Deserialized
deserializeByVersion version =
    if version == "1" then
        Decode.Pathfinder1.decoder

    else
        Json.Decode.fail ("unknown version " ++ version)


fromDeserialized : Plugins -> Deserialized -> Model -> ( Model, List Effect )
fromDeserialized plugins deserialized model =
    let
        groupByNetworkWithField field =
            List.map field
                >> List.Extra.gatherEqualsBy first
                >> List.map (\( fst, more ) -> ( first fst, second fst :: List.map second more ))

        groupByNetwork =
            groupByNetworkWithField .id

        addressesRequests =
            deserialized.addresses
                |> groupByNetwork
                |> List.map
                    (\( currency, addresses ) ->
                        BrowserGotBulkAddresses
                            |> BulkGetAddressEffect
                                { currency = currency
                                , addresses = addresses
                                }
                            |> ApiEffect
                    )

        txsRequests =
            deserialized.txs
                |> groupByNetwork
                |> List.map
                    (\( currency, txs ) ->
                        { deserialized = deserialized
                        , addresses = []
                        , txs = []
                        }
                            |> BrowserGotBulkTxs
                            |> BulkGetTxEffect
                                { currency = currency
                                , txs = txs
                                }
                            |> ApiEffect
                    )

        relationRequests =
            deserialized.aggEdges
                |> List.Extra.gatherEqualsBy .a
                |> List.concatMap
                    (\( parent, children ) ->
                        let
                            onlyIds =
                                parent.b :: List.map .b children
                        in
                        getRelations parent.a Outgoing False onlyIds
                            ++ getRelations parent.a Incoming False onlyIds
                    )

        ( newAndEmptyPathfinder, _ ) =
            Pathfinder.init
                { snapToGrid = Just model.config.snapToGrid
                , highlightClusterFriends = Just model.config.highlightClusterFriends
                , tracingMode = Just model.config.tracingMode
                }
    in
    ( { newAndEmptyPathfinder
        | network =
            ingestAddresses plugins model.config Network.init deserialized.addresses
                |> ingestAggEdges model.config deserialized.aggEdges
        , annotations = List.foldl (\i m -> Annotations.set i.id i.label i.color m) model.annotations deserialized.annotations
        , history = History.init
        , name = deserialized.name
      }
    , txsRequests
        ++ addressesRequests
        ++ relationRequests
    )


autoLoadConversions : Plugins -> Tx -> Model -> ( Model, List Effect )
autoLoadConversions _ tx model =
    case tx.type_ of
        Tx.Utxo _ ->
            n model

        Tx.Account atx ->
            ( model
            , BrowserGotConversions tx
                |> Api.GetConversionEffect
                    { currency = atx.raw.network
                    , txHash = atx.raw.identifier
                    }
                |> ApiEffect
                |> List.singleton
            )


autoLoadAddresses : Plugins -> Bool -> Tx -> Model -> ( Model, List Effect )
autoLoadAddresses plugins autoLinkInTraceMode tx model =
    let
        addresses =
            Tx.listAddressesForTx tx
                |> List.map first

        aggAddressAdd ( d, addressId ) =
            and (loadAddressWithPosition plugins autoLinkInTraceMode (NextTo ( d, addressId )) addressId)

        src =
            if List.member Incoming addresses then
                Nothing

            else
                getAddressForDirection tx Incoming Set.empty
                    |> Maybe.map (Tuple.pair Incoming)

        dst =
            if List.member Outgoing addresses then
                Nothing

            else
                tx
                    |> Tx.getInputAddressIds
                    |> List.map Id.id
                    |> Set.fromList
                    |> getAddressForDirection tx Outgoing
                    |> Maybe.map (Tuple.pair Outgoing)
    in
    [ src, dst ]
        |> List.filterMap identity
        |> List.foldl aggAddressAdd (n model)


updateAddressDetails : Id -> (AddressDetails.Model -> ( AddressDetails.Model, List Effect )) -> Model -> ( Model, List Effect )
updateAddressDetails id upd model =
    case model.details of
        Just (AddressDetails id_ ad) ->
            if id == id_ then
                let
                    ( addressViewDetails, eff ) =
                        upd ad
                in
                ( { model
                    | details =
                        addressViewDetails
                            |> AddressDetails id
                            |> Just
                  }
                , eff
                )

            else
                n model

        _ ->
            n model


upsertTagSummary : Id -> HavingTags -> Dict Id HavingTags -> Dict Id HavingTags
upsertTagSummary id newTagSummary dict =
    if Dict.member id dict then
        Dict.update id
            (Maybe.map
                (\curr ->
                    case ( curr, newTagSummary ) of
                        ( HasTagSummaries _, _ ) ->
                            curr

                        ( HasTagSummaryWithCluster wc, HasTagSummaryWithoutCluster woc ) ->
                            HasTagSummaries { withCluster = wc, withoutCluster = woc }

                        ( HasTagSummaryWithCluster _, HasTagSummaryWithCluster new ) ->
                            HasTagSummaryWithCluster new

                        ( HasTagSummaryWithoutCluster woc, HasTagSummaryWithCluster wc ) ->
                            HasTagSummaries { withCluster = wc, withoutCluster = woc }

                        ( HasTagSummaryWithoutCluster _, HasTagSummaryWithoutCluster new ) ->
                            HasTagSummaryWithoutCluster new

                        ( HasTagSummaryWithCluster x, NoTagsWithoutCluster ) ->
                            HasTagSummaryOnlyWithCluster x

                        ( NoTagsWithoutCluster, HasTagSummaryWithCluster wc ) ->
                            HasTagSummaryOnlyWithCluster wc

                        ( NoTagsWithoutCluster, NoTags ) ->
                            NoTags

                        ( HasTags _, new ) ->
                            new

                        ( LoadingTags, new ) ->
                            new

                        ( NoTags, new ) ->
                            new

                        ( HasExchangeTagOnly, new ) ->
                            new

                        ( _, _ ) ->
                            curr
                )
            )
            dict

    else
        Dict.insert id newTagSummary dict


addOrRemoveTx : Plugins -> Maybe Id -> Id -> Model -> ( Model, List Effect )
addOrRemoveTx plugins addressId txId model =
    Dict.get txId model.network.txs
        |> Maybe.map
            (\t ->
                let
                    delNw =
                        Network.deleteTx txId model.network
                in
                if addressId == Nothing then
                    n { model | network = delNw }

                else
                    Tx.listAddressesForTx t
                        |> List.map second
                        |> List.filterMap
                            (\a ->
                                if Just a.id == addressId then
                                    Nothing

                                else
                                    Dict.get a.id delNw.addresses
                            )
                        |> Network.deleteDanglingAddresses delNw
                        |> flip s_network model
                        |> n
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ -> loadTx True (addressId /= Nothing) plugins txId model)


addMarginPathfinder : BBox -> BBox
addMarginPathfinder bbox =
    { x = bbox.x * unit - unit
    , y = bbox.y * unit - unit * 3
    , width = bbox.width * unit + (2 * unit)
    , height = bbox.height * unit + (8 * unit)
    }
