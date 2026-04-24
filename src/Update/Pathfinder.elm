module Update.Pathfinder exposing (addMarginPathfinder, bboxWithUnit, deserialize, exportGraph, fetchTagSummaryForId, fromDeserialized, multiSearch, removeAddress, removeAggEdge, unselect, update, updateByExportMsg, updateByPluginOutMsg, updateByRoute)

import Animation as A
import Api.Data
import Api.Request.Addresses
import AssocList
import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.Tooltip as Tooltip
import Components.TransactionFilter as TransactionFilter
import Config.Pathfinder exposing (HideForExport(..), TracingMode(..), bulkFetchSizeForExportSize, nodeXOffset)
import Config.Update as Update
import Css.Pathfinder exposing (searchBoxMinWidth)
import Decode.Pathfinder1
import Dict exposing (Dict)
import Effect.Api as Api exposing (Effect(..))
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Effect.Search
import Encode.Pathfinder as Pathfinder
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
import Init.Pathfinder.Table.TagsTable as TagsTable
import Init.Pathfinder.Tx exposing (normalizeUtxo)
import Init.Pathfinder.TxDetails as TxDetails
import Json.Decode
import List.Extra
import Log
import Maybe.Extra
import Model.Dialog as Dialog
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, isInBBox, relativeToGraphZero)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale as Locale
import Model.Notification as Notification
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address as Address exposing (Address, Txs(..), expandAllowed, getAddressType, getTxs, txsSetter)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.CheckingNeighbors as CheckingNeighbors
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.ConversionEdge as ConversionEdge
import Model.Pathfinder.Deserialize exposing (Deserialized)
import Model.Pathfinder.Error exposing (Error(..), InfoError(..))
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id, TxsFilterId(..))
import Model.Pathfinder.Network as Network exposing (FindPosition(..), Network)
import Model.Pathfinder.RelationDetails as RelationDetails
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tools exposing (PointerTool(..), ToolbarHovercardType(..), toolbarHovercardTypeToId)
import Model.Pathfinder.Tx as Tx exposing (Io, Tx)
import Model.Search as Search
import Model.Tx as GTx exposing (parseTxIdentifier)
import Msg.ExportDialog as ExportDialog
import Msg.Pathfinder
    exposing
        ( AddingTxConfig
        , DisplaySettingsMsg(..)
        , Msg(..)
        , OverlayWindows(..)
        )
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.ConversionDetails as ConversionDetails
import Msg.Pathfinder.RelationDetails as RelationDetails
import Msg.Pathfinder.TxDetails as TxDetails
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
import Util.Csv
import Util.Data as Data
import Util.EventualMessages as EventualMessages
import Util.Pathfinder.History as History
import Util.Pathfinder.TagSummary as TagSummary
import Util.TooltipType exposing (TooltipType)
import View.Locale as Locale exposing (makeTimestampFilename)
import View.Pathfinder exposing (originShiftX)
import Workflow


zoomFactor : Float
zoomFactor =
    0.5


relatedAddressesPageSize : Int
relatedAddressesPageSize =
    100


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
        |> and (updateByMsg plugins uc msg)
        |> and syncUrl
        |> and (syncSidePanel uc)
        |> and dispatchEventualMessages
        |> and (closeTooltip msg)


closeTooltip : Msg -> Model -> ( Model, List Effect )
closeTooltip msg model =
    case msg of
        TooltipMsg _ ->
            n model

        _ ->
            Tooltip.close model.tooltip
                |> mapFirst (flip s_tooltip model)
                |> mapSecond (List.map TooltipEffect)


syncUrl : Model -> ( Model, List Effect )
syncUrl model =
    let
        route =
            case model.details of
                Just (AddressDetails id ad) ->
                    let
                        filter =
                            if not ad.transactionsTableOpen then
                                Nothing

                            else
                                case ad.txs of
                                    NotAsked ->
                                        Nothing

                                    Failure _ ->
                                        Nothing

                                    Loading ->
                                        { fromDate = Nothing
                                        , toDate = Nothing
                                        }
                                            |> Just

                                    Success txs ->
                                        txs.filter
                                            |> TransactionFilter.getSettings
                                            |> TransactionFilter.getDateRange
                                            |> Maybe.map
                                                (\( fromDate, toDate ) ->
                                                    { fromDate = fromDate
                                                    , toDate = toDate
                                                    }
                                                )
                    in
                    Route.addressRouteWithFilter
                        { network = Id.network id
                        , address = Id.id id
                        , filter = filter
                        }

                _ ->
                    case model.selection of
                        NoSelection ->
                            Route.Root

                        _ ->
                            model.route
    in
    ( { model | route = route }
    , if model.route /= route then
        route
            |> NavPushRouteEffect
            |> List.singleton

      else
        []
    )


syncSidePanel : Update.Config -> Model -> ( Model, List Effect )
syncSidePanel uc model =
    let
        makeAddressDetails aid =
            Dict.get aid model.network.addresses
                |> Maybe.map (AddressDetails.init (AssocList.get (TxsFilterAddress aid) model.txsFilters))
                |> Maybe.map (AddressDetails aid)

        makeTxDetails tid =
            let
                assets =
                    uc.locale
                        |> flip Locale.getTokenTickers (Id.network tid)

                txsFilter =
                    AssocList.get (TxsFilterTx tid) model.txsFilters
            in
            Dict.get tid model.network.txs
                |> Maybe.map (TxDetails.init txsFilter assets >> TxDetails tid)

        makeRelationDetails rid =
            Dict.get rid model.network.aggEdges
                |> Maybe.andThen
                    (\aggEdge ->
                        let
                            a =
                                Tuple.first rid

                            b =
                                Tuple.second rid

                            addrA =
                                Dict.get a model.network.addresses

                            addrB =
                                Dict.get b model.network.addresses

                            maybeAr1 =
                                addrA
                                    |> Maybe.andThen Address.getActivityRangeAddress
                                    |> Maybe.map (Tuple.mapBoth Time.posixToMillis Time.posixToMillis)

                            maybeAr2 =
                                addrB
                                    |> Maybe.andThen Address.getActivityRangeAddress
                                    |> Maybe.map (Tuple.mapBoth Time.posixToMillis Time.posixToMillis)

                            txsFilterA2b =
                                AssocList.get (TxsFilterAggEdge True rid) model.txsFilters

                            txsFilterB2a =
                                AssocList.get (TxsFilterAggEdge False rid) model.txsFilters
                        in
                        Maybe.map2
                            (\ar1 ar2 ->
                                ( Time.millisToPosix (min (Tuple.first ar1) (Tuple.first ar2))
                                , Time.millisToPosix (max (Tuple.second ar1) (Tuple.second ar2))
                                )
                                    |> RelationDetails.init uc txsFilterA2b txsFilterB2a aggEdge
                                    |> RelationDetails rid
                            )
                            maybeAr1
                            maybeAr2
                    )
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

        ( SelectedAggEdge id, Just (RelationDetails tid d) ) ->
            if id == tid then
                let
                    stillLoading =
                        (RemoteData.isSuccess d.aggEdge.a2b |> not) || (RemoteData.isSuccess d.aggEdge.b2a |> not)
                in
                if stillLoading then
                    case Dict.get tid model.network.aggEdges of
                        Just aggEdge ->
                            -- make sure the aggEdge is up to date, to avoid stale data in RelationDetails view
                            -- needed for asset selection
                            d
                                |> RelationDetails.updateAggEdge uc aggEdge
                                |> RelationDetails tid
                                |> Just

                        Nothing ->
                            model.details

                else
                    model.details

            else
                makeRelationDetails id

        ( SelectedAggEdge id, _ ) ->
            makeRelationDetails id

        ( MultiSelect _, _ ) ->
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
                        Dict.get tid model.network.txs
                            |> Maybe.map
                                (flip s_tx td
                                    >> TxDetails.loadTxDetailsDataAccount
                                    >> mapFirst (TxDetails tid >> Just)
                                )
                            |> Maybe.withDefault (n Nothing)

                    AddressDetails aid ad ->
                        let
                            dateFilterPreset =
                                case model.route of
                                    Route.Network _ (Route.Address _ dateFilter) ->
                                        dateFilter

                                    _ ->
                                        Nothing

                            aidOld =
                                case model.details of
                                    Just (AddressDetails a _) ->
                                        Just a

                                    _ ->
                                        Nothing

                            refreshAddressData =
                                if aidOld /= Just aid then
                                    BrowserGotAddressDataToRefresh
                                        |> Api.GetAddressEffect
                                            { currency = Id.network aid
                                            , address = Id.id aid
                                            , includeActors = True
                                            }
                                        |> ApiEffect
                                        |> List.singleton

                                else
                                    []
                        in
                        Dict.get aid model.network.addresses
                            |> Maybe.map
                                (AddressDetails.syncByAddress uc model.network model.clusters dateFilterPreset ad
                                    >> mapFirst (AddressDetails aid >> Just)
                                    >> mapSecond ((++) refreshAddressData)
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
                    let
                        addressTagsEffect =
                            UserGotDataForTagsListDialog id
                                |> Api.GetAddressTagsEffect
                                    { currency = Id.network id
                                    , address = Id.id id
                                    , pagesize = TagsTable.pagesize
                                    , nextpage = Nothing
                                    , includeBestClusterTag = True
                                    }
                                |> ApiEffect
                    in
                    ( model
                    , [ addressTagsEffect ]
                    )

                AddTags _ ->
                    -- Managed Upstream
                    n model

        UserGotDataForTagsListDialog _ _ ->
            -- handled in src/Update.elm
            n model

        UserGotMoreAddressTagsForDialog _ _ ->
            -- handled in src/Update.elm
            n model

        UserGotClusterTagsForDialog _ _ ->
            -- handled in src/Update.elm
            n model

        UserGotMoreClusterTagsForDialog _ _ ->
            -- handled in src/Update.elm
            n model

        RuntimePostponedUpdateByRoute route ->
            updateByRoute plugins uc route model

        PluginMsg _ ->
            -- handled in src/Update.elm
            n model

        UserClickedExportGraph _ ->
            n model

        BrowserGotTagSummariesForExportGraphTxsAsCSV area includesBestClusterTag tagSummaries ->
            let
                -- Add received tag summaries to model
                ( modelWithTags, _ ) =
                    tagSummaries
                        |> List.foldl
                            (\( id, ts ) ->
                                addTagSummaryToModel includesBestClusterTag id ts
                                    |> and
                            )
                            (n model)

                allAddresses =
                    Dict.values modelWithTags.network.txs
                        |> getToAndFromAddresses uc

                -- Check if all addresses now have their tag summaries loaded
                stillMissing =
                    allAddresses
                        |> List.filter
                            (\( network, addr ) ->
                                Id.init network addr
                                    |> isTagSummaryLoaded True modelWithTags.tagSummaries
                                    |> not
                            )
            in
            -- Only generate export when ALL tag summaries are loaded
            if List.isEmpty stillMissing then
                generateGraphTxsExport uc area modelWithTags

            else
                -- Still waiting for more tag summaries, just update the model
                n modelWithTags

        UserClickedSaveGraph time ->
            ( model
            , [ (case time of
                    Nothing ->
                        Time.now
                            |> Task.perform (Just >> UserClickedSaveGraph)

                    Just t ->
                        Pathfinder.encode model
                            |> pair
                                (makeTimestampFilename uc.locale t
                                    |> (\tt -> tt ++ ".gs")
                                )
                            |> Ports.serialize
                )
                    |> Pathfinder.CmdEffect
              ]
            )
                |> and setClean

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

        BrowserGotAddressDataToRefresh data ->
            let
                id =
                    Id.init data.currency data.address
            in
            model.network
                |> Network.updateAddress id (s_data (Success data))
                |> flip s_network model
                |> updateAddressDetails id
                    (\ad ->
                        n
                            { ad
                                | address = s_data (Success data) ad.address
                            }
                    )

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

        BrowserGotAddressPubkeyRelations id x ->
            let
                modelWithRelations =
                    updateAddressRelatedData id x model

                nextFetch =
                    x.nextPage
                        |> Maybe.map (\nextPage -> fetchAddressPubkeyRelations id (Just nextPage))
                        |> Maybe.Extra.toList
            in
            ( modelWithRelations, nextFetch )

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

        BrowserGotClusterData _ data ->
            let
                clusterId =
                    Id.initClusterId data.currency data.entity

                setServiceType addr =
                    Just data
                        |> getAddressType addr
                        |> flip s_addressServiceType addr
            in
            n
                { model
                    | clusters = Dict.insert clusterId (Success data) model.clusters
                    , network =
                        Network.updateAddressesByClusterId clusterId setServiceType model.network
                }

        SearchMsg m ->
            case m of
                Search.BrowserGotMultiSearchResult _ result ->
                    let
                        -- Compute viewport center in graph coordinates for placing new nodes
                        viewportCenter =
                            Transform.getCurrent model.transform
                                |> (\t -> AtViewportCenter (t.x / unit) (t.y / unit))

                        addAccAddr aId ( x, eff ) =
                            loadAddressWithPosition plugins True viewportCenter aId x |> Tuple.mapSecond ((++) eff)

                        addAccTxs aId ( x, eff ) =
                            loadTxWithPosition viewportCenter True True plugins aId x |> Tuple.mapSecond ((++) eff)

                        addressesToAdd =
                            result.currencies
                                |> List.concatMap
                                    (\c ->
                                        c.addresses
                                            |> List.map (Tuple.pair c.currency)
                                    )
                                |> List.map (\( currency, addr ) -> Id.init currency addr)

                        txsToAdd =
                            result.currencies
                                |> List.concatMap
                                    (\c ->
                                        c.txs
                                            |> List.map (Tuple.pair c.currency)
                                    )
                                |> List.map (\( currency, txh ) -> Id.init currency txh)

                        modelWithTxsAdded =
                            txsToAdd |> List.foldl addAccTxs ( model, [] )
                    in
                    addressesToAdd
                        |> List.foldl addAccAddr modelWithTxsAdded
                        |> and (setDirty True)

                Search.UserClicksResultLine ->
                    let
                        query =
                            Search.query model.search

                        selectedValue =
                            Search.selectedValue model.search

                        ( search, eff ) =
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
                                    |> flip (::)
                                        (List.map Pathfinder.SearchEffect eff)
                                    |> Tuple.pair m2

                            Nothing ->
                                ( m2, List.map Pathfinder.SearchEffect eff )
                                    |> and (multiSearch query)

                _ ->
                    Search.update m model.search
                        |> Tuple.mapFirst (\s -> s_search s model)
                        |> Tuple.mapSecond (List.map Pathfinder.SearchEffect)

        UserClosedDetailsView ->
            { model | details = Nothing, selection = NoSelection }
                |> n

        TxDetailsMsg (TxDetails.UserClickedTxInSubTxsTable tx) ->
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

                RelationDetails.ExportCSVMsg isA2b tbl ms ->
                    let
                        config =
                            RelationDetails.makeExportCSVConfig uc isA2b id tbl

                        ( exportCSV, eff ) =
                            ExportCSV.update ms config model.exportCSV
                    in
                    ( { model | exportCSV = exportCSV }
                    , eff
                    )

                RelationDetails.BrowserGotLinksForExport isA2b tbl data ->
                    let
                        config =
                            RelationDetails.makeExportCSVConfig uc isA2b id tbl

                        ( exportCSV, eff ) =
                            ExportCSV.gotData uc config ( data.links, data.nextPage ) model.exportCSV
                    in
                    ( { model | exportCSV = exportCSV }
                    , eff
                    )

                RelationDetails.TooltipMsg tm ->
                    handleTooltipMsg tm model

                _ ->
                    n model
            )
                |> and (updateRelationDetails uc id submsg)

        AddressDetailsMsg addressId subm ->
            let
                fetchTagSummariesForNeigbors neighbors =
                    let
                        network =
                            Id.network addressId
                    in
                    neighbors
                        |> List.map (.address >> .address)
                        |> fetchTagSummaryForIds True model.tagSummaries BrowserGotTagSummaries network
                        |> pair model
                        |> and
                            (AddressDetails.update uc subm
                                |> updateAddressDetails addressId
                            )
            in
            case subm of
                AddressDetails.GotNeighborsForAddressDetails _ _ { neighbors } ->
                    fetchTagSummariesForNeigbors neighbors

                AddressDetails.BrowserGotAddressesForTags _ addresses ->
                    let
                        network =
                            Id.network addressId
                    in
                    addresses
                        |> List.map .address
                        |> fetchTagSummaryForIds False model.tagSummaries BrowserGotTagSummaries network
                        |> pair model
                        |> and
                            (AddressDetails.update uc subm
                                |> updateAddressDetails addressId
                            )

                AddressDetails.BrowserGotPubkeyRelations x ->
                    updateAddressRelatedData addressId x model
                        |> updateAddressDetails addressId
                            (AddressDetails.update uc subm)

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

                AddressDetails.ExportCSVMsg table ms ->
                    let
                        config =
                            AddressDetails.makeExportCSVConfig uc addressId table

                        ( exportCSV, eff ) =
                            ExportCSV.update ms config model.exportCSV
                    in
                    ( { model | exportCSV = exportCSV }
                    , eff
                    )

                AddressDetails.GotAddressTxsForExport table data ->
                    let
                        nw =
                            Id.network addressId
                    in
                    if nw |> Data.isAccountLike then
                        let
                            addressTxs =
                                data.addressTxs
                                    |> List.filterMap
                                        (\tx ->
                                            case tx of
                                                Api.Data.AddressTxAddressTxUtxo _ ->
                                                    Nothing

                                                Api.Data.AddressTxTxAccount t ->
                                                    Just t
                                        )
                        in
                        getTagsForExport addressId table ( addressTxs, data.nextPage ) model

                    else
                        let
                            addressTxs =
                                data.addressTxs
                                    |> List.filterMap
                                        (\tx ->
                                            case tx of
                                                Api.Data.AddressTxAddressTxUtxo t ->
                                                    Just t

                                                Api.Data.AddressTxTxAccount _ ->
                                                    Nothing
                                        )
                        in
                        ( model
                        , AddressDetails.BrowserGotBulkTxsForExport table addressTxs data.nextPage 0 []
                            >> AddressDetailsMsg addressId
                            |> BulkGetTxEffect
                                { currency = nw
                                , txs =
                                    List.map .txHash addressTxs
                                        |> List.take bulkFetchSizeForExportSize
                                }
                            |> ApiEffect
                            |> List.singleton
                        )

                AddressDetails.BrowserGotBulkTxsForExport table addressTxs nextPage fetchedIOprev fetched txs ->
                    let
                        all =
                            fetched ++ txs

                        fetchedSize =
                            List.length all

                        addressTxsDict =
                            addressTxs
                                |> List.map (\t -> ( t.txHash, t ))
                                |> Dict.fromList

                        fetchedIO =
                            txs
                                |> List.foldl
                                    (\( txHash, tx ) sum ->
                                        case tx of
                                            Api.Data.TxTxUtxo t ->
                                                Dict.get txHash addressTxsDict
                                                    |> Maybe.andThen
                                                        (\atx ->
                                                            if atx.value.value > 0 then
                                                                t.inputs

                                                            else
                                                                t.outputs
                                                        )
                                                    |> Maybe.map
                                                        (List.filterMap
                                                            (.address >> List.head)
                                                            >> Set.fromList
                                                        )
                                                    |> Maybe.map Set.size
                                                    |> Maybe.withDefault 0
                                                    |> (+) sum

                                            Api.Data.TxTxAccount _ ->
                                                sum
                                    )
                                    0
                                |> (+) fetchedIOprev

                        config =
                            AddressDetails.makeExportCSVConfig uc addressId table

                        nextTxs =
                            List.map .txHash addressTxs
                                |> List.drop fetchedSize
                                |> List.take bulkFetchSizeForExportSize
                    in
                    if fetchedIO >= ExportCSV.getNumberOfRows config || List.isEmpty nextTxs then
                        let
                            merged =
                                all
                                    |> mergeAddressTxsAndTxs uc (Id.id addressId) addressTxs
                        in
                        getTagsForExport addressId table ( merged, nextPage ) model

                    else
                        ( model
                        , AddressDetails.BrowserGotBulkTxsForExport table addressTxs nextPage fetchedIO all
                            >> AddressDetailsMsg addressId
                            |> BulkGetTxEffect
                                { currency = Id.network addressId
                                , txs = nextTxs
                                }
                            |> ApiEffect
                            |> List.singleton
                        )

                AddressDetails.BrowserGotBulkTagsForExport table data includesBestClusterTag tagSummeries ->
                    let
                        -- throw away the fetch actor effects
                        ( modelWithTags, _ ) =
                            tagSummeries
                                |> List.foldl
                                    (\( id, ts ) ->
                                        addTagSummaryToModel includesBestClusterTag id ts
                                            |> and
                                    )
                                    (n model)

                        dataWithTags =
                            data
                                |> mapFirst (addFeeRows uc addressId)
                                |> mapFirst
                                    (List.map
                                        (\tx ->
                                            ( tx
                                            , Id.init tx.currency tx.fromAddress
                                                |> getHavingTags modelWithTags
                                                |> getTagsummary
                                            , Id.init tx.currency tx.toAddress
                                                |> getHavingTags modelWithTags
                                                |> getTagsummary
                                            )
                                        )
                                    )

                        config =
                            AddressDetails.makeExportCSVConfig uc addressId table

                        ( exportCSV, eff ) =
                            ExportCSV.gotData uc config dataWithTags model.exportCSV
                    in
                    ( { model | exportCSV = exportCSV }
                    , eff
                    )

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
                unselect m1

            else
                n m1

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
                                crd =
                                    case tm.state of
                                        Transform.Settled c ->
                                            c

                                        Transform.Transitioning v ->
                                            v.from

                                z =
                                    value crd.z

                                bbox =
                                    { x =
                                        ((Basics.min start.x now.x + originShiftX) * z) + crd.x
                                    , y =
                                        (Basics.min start.y now.y * z) + crd.y
                                    , width =
                                        abs (start.x - now.x) * z
                                    , height =
                                        abs (start.y - now.y) * z
                                    }

                                isInBBoxTx =
                                    Tx.getCoords
                                        >> Maybe.map (coordsWithUnit >> isInBBox bbox)
                                        >> Maybe.withDefault False

                                isInBBoxAddr =
                                    Address.getCoords
                                        >> coordsWithUnit
                                        >> isInBBox bbox

                                selectedTxs =
                                    List.filter isInBBoxTx (Dict.values model.network.txs) |> List.map (.id >> MSelectedTx)

                                selectedAdr =
                                    List.filter isInBBoxAddr (Dict.values model.network.addresses) |> List.map (.id >> MSelectedAddress)

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
                            (if model.config.snapToGrid then
                                network |> Network.snapToGrid

                             else
                                network
                            )
                                |> (if model.config.avoidOverlapingNodes then
                                        Network.resolveOverlapsExcept Network.Compact (Just id)

                                    else
                                        identity
                                   )
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
            , []
            )

        UserPushesRightMouseButtonOnGraph coords ->
            ( { model
                | pointerTool = Select
                , dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            Dragging model.transform (relativeToGraphZero uc.size coords) (relativeToGraphZero uc.size coords)

                        _ ->
                            NoDragging
              }
            , []
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
            , []
            )

        UserMovesMouseOverTx id ->
            handleTxHover id model

        UserMovesMouseOverAddress id ->
            if model.hovered == HoveredAddress id then
                n model

            else
                let
                    showHover _ =
                        let
                            unhovered =
                                unhover model

                            nw2 =
                                unhovered.network.addresses
                                    |> Dict.get id
                                    |> Maybe.andThen Address.getClusterId
                                    |> Maybe.map (\e -> Network.updateAddressesByClusterId e (s_clusterSiblingHovered True) unhovered.network)
                                    |> Maybe.withDefault model.network
                        in
                        { unhovered
                            | hovered = HoveredAddress id
                            , network = nw2
                        }
                in
                case model.details of
                    Just (AddressDetails aid _) ->
                        if id /= aid then
                            showHover () |> n

                        else
                            n model

                    _ ->
                        showHover () |> n

        UserMovesMouseOutAddress _ ->
            unhover model
                |> n

        UserMovesMouseOutTx _ ->
            unhover model |> n

        UserPushesLeftMouseButtonOnUtxoTx id coords ->
            ( { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            DraggingNode id coords coords

                        _ ->
                            model.dragging
              }
            , []
            )

        UserMovesMouseOnGraph coords ->
            case model.dragging of
                NoDragging ->
                    ( model, [] )

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
                    ( { model
                        | network = network
                        , dragging = DraggingNode id start coords
                      }
                    , [ RepositionTooltipEffect ]
                    )

        AnimationFrameDeltaForTransform delta ->
            ( { model
                | transform = Transform.transition delta model.transform
              }
            , [ RepositionTooltipEffect ]
            )

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

        UserClickedAddressExpandHandleInIoTable txId addressId direction ->
            if Network.hasAddress addressId model.network then
                ( model, [ InternalEffect (InternalExpandSpecificTxAndAddress txId addressId direction) ] )

            else
                let
                    ( eventualMessagesNew, mcmd ) =
                        model.eventualMessages
                            |> EventualMessages.addMessage
                                (Network.AddressIsLoaded addressId)
                                (InternalExpandSpecificTxAndAddress txId addressId direction)

                    getAddressEffect =
                        InternalEffect
                            (UserClickedAddressCheckboxInTable addressId)

                    cmdEffect =
                        mcmd |> Maybe.map (CmdEffect >> List.singleton) |> Maybe.withDefault []
                in
                ( model
                    |> s_eventualMessages eventualMessagesNew
                , getAddressEffect :: cmdEffect
                )

        InternalExpandSpecificTxAndAddress txId addressId direction ->
            Dict.get txId model.network.txs
                |> Maybe.andThen (Tx.getUtxoTx >> Maybe.map .raw)
                |> Maybe.map
                    (\utxo ->
                        let
                            config =
                                { addressId = addressId
                                , direction = direction
                                , allowMultiple = False
                                }
                        in
                        WorkflowNextUtxoTx.start config utxo
                            |> Workflow.mapEffect (WorkflowNextUtxoTx config Nothing)
                            |> Workflow.next
                            |> List.map ApiEffect
                            |> pair model
                    )
                |> Maybe.withDefault (n model)

        UserPressedArrowKey direction ->
            case model.selection of
                SelectedAddress id ->
                    Dict.get id model.network.addresses
                        |> Maybe.map
                            (\address ->
                                case getTxs address direction of
                                    Txs _ ->
                                        focusNeighborAddress uc id direction model

                                    _ ->
                                        update plugins uc (UserClickedAddressExpandHandle id direction) model
                            )
                        |> Maybe.withDefault (n model)

                _ ->
                    n model

        UserClickedAddress id ->
            if model.modPressed || model.pointerTool == Select then
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
                            InfiniteTable.getPage t
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

                unsupported =
                    conversions
                        |> List.filter (\c -> not c.toIsSupportedAsset || not c.fromIsSupportedAsset)
                        |> List.map
                            (\c ->
                                { toAddress = c.toAddress
                                , toNetwork = c.toNetwork
                                , conversionType = c.conversionType
                                }
                            )

                supportedConversions =
                    conversions
                        |> List.filter (\c -> c.toIsSupportedAsset && c.fromIsSupportedAsset)

                networkWithFlag =
                    if List.isEmpty unsupported then
                        model.network

                    else
                        Network.updateTx txid (\t -> { t | unsupportedConversions = unsupported }) model.network

                modelWithFlag =
                    { model | network = networkWithFlag }
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
                    ( modelWithFlag, [] )

        BrowserGotTx ({ requestedTxHash } as loadTxConfig) tx ->
            let
                txId =
                    Tx.getTxId tx

                network =
                    Id.network txId
            in
            if Dict.member txId model.network.txs then
                n model

            else if Data.isAccountLike network && Id.id txId /= requestedTxHash && Tx.isZeroValueTx tx then
                -- a tx hash without subtx id part was requested
                ( model
                , BrowserGotTxFlow loadTxConfig tx
                    |> Api.ListTxFlowsEffect
                        { currency = network
                        , txHash = requestedTxHash
                        , includeZeroValueSubTxs = False
                        , pagesize = Just 1
                        , token_currency = Nothing
                        , nextpage = Nothing
                        }
                    |> ApiEffect
                    |> List.singleton
                )
                    |> and (selectTx txId)

            else
                browserGotTx plugins uc loadTxConfig tx model

        BrowserGotTxFlow loadTxConfig originalTx txs ->
            txs.nextPage
                |> Maybe.map
                    (\_ ->
                        browserGotTx plugins uc loadTxConfig originalTx model
                    )
                |> Maybe.Extra.orElseLazy
                    (\_ ->
                        txs.txs
                            |> List.head
                            |> Maybe.map
                                (\tx ->
                                    n model
                                        |> and (selectTx (Tx.getTxId tx))
                                        |> and (browserGotTx plugins uc loadTxConfig tx)
                                )
                    )
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        browserGotTx plugins uc loadTxConfig originalTx model
                    )

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

                UserClickedToggleBothValueDisplay ->
                    -- handled Upstream
                    n model

                UserClickedToggleValueDetail ->
                    -- handled Upstream
                    n model

                UserClickedToggleShowHash ->
                    -- handled Upstream
                    n model

                UserClickedToggleAvoidOverlapingNodes ->
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
            List.foldl
                (\( id, ts ) ->
                    addTagSummaryToModel includesBestClusterTag id ts
                        |> and
                )
                (n model)
                data

        BrowserGotTagSummary includesBestClusterTag id data ->
            addTagSummaryToModel includesBestClusterTag id data model

        BrowserGotClusterTagsProbe id hasClusterTags ->
            if hasClusterTags then
                let
                    updatedTagSummaries =
                        upsertTagSummary id HasClusterTagsOnlyButNoDirect model.tagSummaries
                in
                ( { model | tagSummaries = updatedTagSummaries }
                    |> updateTagDataOnAddress id
                , []
                )

            else
                n model

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

                updatedModel =
                    { model | tagSummaries = tagSummaries }
                        |> (\m ->
                                data
                                    |> List.map Tuple.first
                                    |> Set.fromList
                                    |> Set.foldl updateTagDataOnAddress m
                           )
            in
            ( updatedModel, [] )

        UserClickedToolbarDeleteIcon ->
            deleteSelection model

        UserClickedContextMenuDeleteIcon menuType ->
            case menuType of
                ContextMenu.AddressContextMenu _ ->
                    -- removeAddress id model
                    deleteSelection model

                ContextMenu.TransactionContextMenu _ ->
                    -- removeTx id model
                    deleteSelection model

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

        UserInputsAnnotation ids str ->
            n { model | annotations = List.foldl (\id ann -> Annotations.setLabel id str ann) model.annotations ids }

        UserSelectsAnnotationColor ids clr ->
            n { model | annotations = List.foldl (\id ann -> Annotations.setColor id clr ann) model.annotations ids }

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

        UserClickedContextMenuAlignHorizontally ->
            case model.selection of
                MultiSelect selections ->
                    let
                        -- Collect y coordinates from selected addresses and transactions
                        getYCoords sel =
                            case sel of
                                MSelectedAddress id ->
                                    Dict.get id model.network.addresses
                                        |> Maybe.map (.y >> A.getTo)

                                MSelectedTx id ->
                                    Dict.get id model.network.txs
                                        |> Maybe.map (.y >> A.getTo)

                        yCoords =
                            selections
                                |> List.filterMap getYCoords
                                |> List.sort

                        -- Calculate median y coordinate
                        medianY =
                            let
                                len =
                                    List.length yCoords
                            in
                            if len == 0 then
                                0

                            else if modBy 2 len == 1 then
                                -- Odd number: take middle element
                                List.drop (len // 2) yCoords
                                    |> List.head
                                    |> Maybe.withDefault 0

                            else
                                -- Even number: average of two middle elements
                                let
                                    mid1 =
                                        List.drop (len // 2 - 1) yCoords |> List.head |> Maybe.withDefault 0

                                    mid2 =
                                        List.drop (len // 2) yCoords |> List.head |> Maybe.withDefault 0
                                in
                                (mid1 + mid2) / 2

                        -- Move each selected node to the median y coordinate
                        moveToMedianY sel net =
                            case sel of
                                MSelectedAddress id ->
                                    Network.updateAddress id (Node.setY medianY) net

                                MSelectedTx id ->
                                    Network.updateTx id (Node.setY medianY) net

                        newNetwork =
                            List.foldl moveToMedianY model.network selections
                                |> Network.resolveOverlaps Network.Spacious
                    in
                    n { model | network = newNetwork, contextMenu = Nothing }

                _ ->
                    n { model | contextMenu = Nothing }

        UserClickedContextMenuAlignVertically ->
            case model.selection of
                MultiSelect selections ->
                    let
                        -- Collect x coordinates from selected addresses and transactions
                        getXCoords sel =
                            case sel of
                                MSelectedAddress id ->
                                    Dict.get id model.network.addresses
                                        |> Maybe.map .x

                                MSelectedTx id ->
                                    Dict.get id model.network.txs
                                        |> Maybe.map .x

                        xCoords =
                            selections
                                |> List.filterMap getXCoords
                                |> List.sort

                        -- Calculate median x coordinate
                        medianX =
                            let
                                len =
                                    List.length xCoords
                            in
                            if len == 0 then
                                0

                            else if modBy 2 len == 1 then
                                -- Odd number: take middle element
                                List.drop (len // 2) xCoords
                                    |> List.head
                                    |> Maybe.withDefault 0

                            else
                                -- Even number: average of two middle elements
                                let
                                    mid1 =
                                        List.drop (len // 2 - 1) xCoords |> List.head |> Maybe.withDefault 0

                                    mid2 =
                                        List.drop (len // 2) xCoords |> List.head |> Maybe.withDefault 0
                                in
                                (mid1 + mid2) / 2

                        -- Move each selected node to the median x coordinate
                        moveToMedianX sel net =
                            case sel of
                                MSelectedAddress id ->
                                    Network.updateAddress id (Node.setX medianX) net

                                MSelectedTx id ->
                                    Network.updateTx id (Node.setX medianX) net

                        newNetwork =
                            List.foldl moveToMedianX model.network selections
                                |> Network.resolveOverlaps Network.Spacious
                    in
                    n { model | network = newNetwork, contextMenu = Nothing }

                _ ->
                    n { model | contextMenu = Nothing }

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
            , []
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
                    hovered _ =
                        let
                            unhovered =
                                unhover model
                        in
                        { unhovered
                            | network = Network.updateAggEdge id (s_hovered True) unhovered.network
                            , hovered = HoveredAggEdge id
                        }
                in
                case model.details of
                    Just (RelationDetails rid _) ->
                        if id /= rid then
                            hovered () |> n

                        else
                            n model

                    _ ->
                        hovered () |> n

        UserMovesMouseOutAggEdge _ ->
            ( unhover model
            , []
            )

        InternalExportGraphTxsCompleted ->
            -- handled upstream
            n model

        InternalChangedTxFilter id filter ->
            n { model | txsFilters = AssocList.insert id filter model.txsFilters }

        InternalHoveredQuickFilter qf ->
            qf
                |> Maybe.map TransactionFilter.getTxIdFromQuickFilter
                |> Maybe.map (flip handleTxHover model)
                |> Maybe.Extra.withDefaultLazy (\_ -> unhover model |> n)

        TransactionFilterMsg tm ->
            case model.details of
                Just (TxDetails tid txDetailsModel) ->
                    TransactionFilter.update tm txDetailsModel.subTxsTableFilter
                        |> mapFirst (flip s_subTxsTableFilter txDetailsModel >> TxDetails tid >> Just >> flip s_details model)
                        |> mapSecond (List.map TransactionFilterEffect)

                Just (AddressDetails aid addressDetailsModel) ->
                    case addressDetailsModel.txs of
                        RemoteData.Success txsModel ->
                            TransactionFilter.update tm txsModel.filter
                                |> mapFirst
                                    (flip s_filter txsModel
                                        >> RemoteData.Success
                                        >> flip s_txs addressDetailsModel
                                        >> AddressDetails aid
                                        >> Just
                                        >> flip s_details model
                                    )
                                |> mapSecond (List.map TransactionFilterEffect)

                        _ ->
                            n model

                Just (RelationDetails _ _) ->
                    -- ignore for now
                    -- we need a way of handling txfilter effects separately for a2b and b2a table
                    n model

                _ ->
                    n model

        TooltipMsg tm ->
            handleTooltipMsg tm model

        RepositionTooltip ->
            ( model
            , Tooltip.reposition model.tooltip
                |> List.map TooltipEffect
            )


handleTooltipMsg : Tooltip.Msg TooltipType -> Model -> ( Model, List Effect )
handleTooltipMsg tm model =
    let
        ( tooltipModel, eff ) =
            Tooltip.update tm model.tooltip
    in
    ( { model | tooltip = tooltipModel }
    , List.map TooltipEffect eff
    )


multiSearch : String -> Model -> ( Model, List Effect )
multiSearch query model =
    let
        multiInputList =
            Data.parseMultiIdentifierInput query
    in
    if List.length multiInputList > 1 then
        ( model
        , multiInputList
            |> List.map
                (\inp ->
                    Pathfinder.SearchEffect
                        (Effect.Search.SearchEffect
                            { query = inp
                            , currency = Nothing
                            , limit = Just 1
                            , config =
                                Api.defaultSearchConfig
                                    |> s_includeAddresses (Just True)
                                    |> s_includeTxs (Just True)
                                    |> s_includeActors (Just False)
                                    |> s_includeLabels (Just False)
                            , toMsg = Search.BrowserGotMultiSearchResult query
                            }
                        )
                )
        )

    else
        n model


exportGraph : Dialog.ExportConfig msg -> Maybe BBox -> Model -> ( Model, List Effect )
exportGraph conf bbox model =
    ( model.config
        |> s_hideForExport (Exporting <| not conf.keepSelectionHighlight)
        |> flip s_config model
        |> s_exportImage (Just ExportingImage)
    , [ { filename = conf.filename
        , graphId = graphId
        , viewbox = bbox |> Maybe.map addMarginForExport
        , transparentBackground = conf.transparentBackground
        }
            |> Ports.exportGraph
            |> Pathfinder.CmdEffect
      ]
    )


browserGotTx : Plugins -> Update.Config -> AddingTxConfig -> Api.Data.Tx -> Model -> ( Model, List Effect )
browserGotTx plugins uc { pos, loadAddresses, autoLinkInTraceMode } tx model =
    if Dict.member (Tx.getTxId tx) model.network.txs then
        n model

    else
        let
            ( newTx, newNetwork ) =
                Network.addTxWithPosition model.config pos tx model.network
        in
        model
            |> s_network newNetwork
            |> checkSelection uc
            |> and
                (if loadAddresses then
                    autoLoadAddresses plugins autoLinkInTraceMode newTx

                 else
                    n
                )
            |> and (autoLoadConversions plugins newTx)


addFeeRows : Update.Config -> Id -> List Api.Data.TxAccount -> List Api.Data.TxAccount
addFeeRows uc addressId =
    List.concatMap
        (\tx ->
            tx
                :: (tx.fee
                        |> Maybe.map
                            (\fee ->
                                if tx.fromAddress == Id.id addressId then
                                    [ { tx
                                        | value = Data.negateValues fee
                                        , toAddress = Locale.string uc.locale "fee"
                                      }
                                    ]

                                else
                                    []
                            )
                        |> Maybe.withDefault []
                   )
        )


{-| Normalize address txs to account tx schema
-}
mergeAddressTxsAndTxs : Update.Config -> String -> List Api.Data.AddressTxUtxo -> List ( String, Api.Data.Tx ) -> List Api.Data.TxAccount
mergeAddressTxsAndTxs uc address addressTxs txs =
    let
        dict =
            Dict.fromList txs
    in
    addressTxs
        |> List.filterMap
            (\addressTx ->
                Dict.get addressTx.txHash dict
                    |> Maybe.andThen
                        (\t ->
                            case t of
                                Api.Data.TxTxUtxo tx ->
                                    Just <| normalizeUtxo tx

                                Api.Data.TxTxAccount _ ->
                                    Nothing
                        )
                    |> Maybe.map
                        (\{ inputs, outputs } ->
                            let
                                sumInputs =
                                    Dict.values inputs
                                        |> List.map .values
                                        |> Data.sumValues

                                addressTxPortion =
                                    toFloat addressTx.value.value / toFloat sumInputs.value
                            in
                            if addressTx.value.value > 0 then
                                let
                                    inputToTxAccount : ( Id, Io ) -> Api.Data.TxAccount
                                    inputToTxAccount ( addr, io ) =
                                        { contractCreation = Nothing
                                        , currency = addressTx.currency
                                        , fromAddress = Id.id addr
                                        , height = addressTx.height
                                        , identifier = ""
                                        , isExternal = Nothing
                                        , network = addressTx.currency
                                        , timestamp = addressTx.timestamp
                                        , toAddress = address
                                        , tokenTxId = Nothing
                                        , txHash = addressTx.txHash
                                        , txType = "utxo"
                                        , value = Data.mulValues addressTxPortion io.values
                                        , fee = Nothing
                                        }
                                in
                                inputs
                                    |> Dict.toList
                                    |> List.map inputToTxAccount

                            else
                                let
                                    sumOutputs =
                                        Dict.values outputs
                                            |> List.map .values
                                            |> Data.sumValues

                                    fee =
                                        Data.subValues sumInputs sumOutputs

                                    outputToTxAccount : ( Id, Io ) -> Api.Data.TxAccount
                                    outputToTxAccount ( addr, io ) =
                                        { contractCreation = Nothing
                                        , currency = addressTx.currency
                                        , fromAddress = address
                                        , height = addressTx.height
                                        , identifier = ""
                                        , isExternal = Nothing
                                        , network = addressTx.currency
                                        , timestamp = addressTx.timestamp
                                        , toAddress = Id.id addr
                                        , tokenTxId = Nothing
                                        , txHash = addressTx.txHash
                                        , txType = "utxo"
                                        , value = Data.mulValues addressTxPortion io.values
                                        , fee = Nothing
                                        }
                                in
                                outputs
                                    |> Dict.toList
                                    |> List.map outputToTxAccount
                                    |> flip (++)
                                        [ outputToTxAccount
                                            ( Locale.string uc.locale "fee"
                                                |> Id.init addressTx.currency
                                            , { address = Nothing
                                              , values = fee
                                              , aggregatesN = 1
                                              }
                                            )
                                        ]
                        )
            )
        |> List.concat


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
                        |> notify "Removed-transactions"
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
                    |> notify "Added-transactions"
                )


setTracingMode : TracingMode -> Model -> ( Model, List Effect )
setTracingMode tm model =
    s_tracingMode tm model.config
        |> flip s_config model
        |> n


updateRelationDetails : Update.Config -> ( Id, Id ) -> RelationDetails.Msg -> Model -> ( Model, List Effect )
updateRelationDetails uc id msg model =
    getRelationDetails model id
        |> Maybe.map
            (\rdModel ->
                let
                    ( nVs, eff ) =
                        RelationDetails.update uc id msg rdModel
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


handleTx : Plugins -> Update.Config -> { a | direction : Direction, addressId : Id } -> Maybe Id -> Api.Data.Tx -> Model -> ( Model, List Effect )
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
            let
                hasIncomingAnchorAdjacency =
                    GTx.hasAddress Incoming (Id.id config.addressId) tx

                hasOutgoingAnchorAdjacency =
                    GTx.hasAddress Outgoing (Id.id config.addressId) tx
            in
            if
                hasIncomingAnchorAdjacency
                    || hasOutgoingAnchorAdjacency
            then
                addTx plugins uc config.addressId config.direction Nothing tx model

            else
                ( model
                    |> s_network (Network.updateAddress config.addressId (Txs Set.empty |> txsSetter config.direction) model.network)
                , NoAdjaccentTxForAddressFound config.addressId
                    |> InfoError
                    |> ErrorEffect
                    |> List.singleton
                )


placeNeighborIfError : Plugins -> Update.Config -> { a | direction : Direction, addressId : Id } -> Id -> Model -> ( Model, List Effect )
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

        clusterColor =
            Colors.getAssignedColor Colors.Clusters clusterId ncolors
                |> Maybe.map .color

        ( newAddress, net ) =
            Network.addAddressWithPosition plugins model.config position id model.network
                |> mapSecond (Network.updateAddress id (s_data (Success data)))
                |> mapSecond (Network.updateAddressesByClusterId clusterId (s_clusterColor clusterColor))

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
            case position of
                AtViewportCenter _ _ ->
                    -- Don't move the viewport when adding at viewport center
                    model.transform

                _ ->
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
                ++ [ fetchAddressPubkeyRelations id Nothing
                   , InternalEffect (InternalPathfinderAddedAddress newAddress.id)
                   ]
            )
        |> and (checkSelection uc)


fetchAddressPubkeyRelations : Id -> Maybe String -> Effect
fetchAddressPubkeyRelations id nextpage =
    BrowserGotAddressPubkeyRelations id
        |> Api.ListRelatedAddressesEffect
            { currency = Id.network id
            , address = Id.id id
            , reltype = Api.Request.Addresses.AddressRelationTypePubkey
            , pagesize = relatedAddressesPageSize
            , nextpage = nextpage
            }
        |> ApiEffect


updateAddressRelatedData : Id -> Api.Data.RelatedAddresses -> Model -> Model
updateAddressRelatedData id x model =
    { model
        | network =
            Network.updateAddress id
                (\address ->
                    let
                        withRelatedAddresses =
                            x.relatedAddresses
                                |> List.foldl
                                    (\related acc ->
                                        Dict.update related.currency
                                            (Maybe.map (Set.insert related.address)
                                                >> Maybe.withDefault (Set.singleton related.address)
                                                >> Just
                                            )
                                            acc
                                    )
                                    address.networks

                        withCurrentAddress =
                            Dict.update (Id.network id)
                                (Maybe.map (Set.insert (Id.id id))
                                    >> Maybe.withDefault (Set.singleton (Id.id id))
                                    >> Just
                                )
                                withRelatedAddresses
                    in
                    { address
                        | networks = withCurrentAddress
                    }
                )
                model.network
    }


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
    if model.modPressed || model.pointerTool == Select then
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
                            |> bboxWithUnit
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
                    , allowMultiple = False
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

        cluster =
            Dict.get addressId m.network.addresses
                |> Maybe.andThen Address.getClusterId
                |> Maybe.andThen (flip Dict.get m.clusters)
                |> Maybe.andThen RemoteData.toMaybe

        updateTagsummaryData clusterOnly tagdata =
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
                |> Network.updateAddress addressId (s_hasTags (tagdata.tagCount > 0 && not (TagSummary.hasOnlyExchangeTags tagdata) && not clusterOnly))
                |> Network.updateAddress addressId
                    (\t ->
                        { t
                            | hasClusterTagsOnly = clusterOnly && not (TagSummary.hasOnlyExchangeTags tagdata)
                        }
                    )
                |> Network.updateAddress addressId (s_actor actorlabel)
                |> Network.updateAddress addressId
                    (\addr ->
                        { addr | addressServiceType = getAddressType addr cluster }
                    )

        net td =
            case td of
                HasTagSummaries { withCluster } ->
                    updateTagsummaryData False withCluster

                HasTagSummaryWithCluster ts ->
                    updateTagsummaryData False ts

                HasTagSummaryOnlyWithCluster ts ->
                    updateTagsummaryData True ts

                HasTagSummaryWithoutCluster ts ->
                    updateTagsummaryData False ts

                HasExchangeTagOnly ->
                    m.network
                        |> Network.updateAddress addressId (s_hasTags False)
                        |> Network.updateAddress addressId (\t -> { t | hasClusterTagsOnly = False })

                HasClusterTagsOnlyButNoDirect ->
                    m.network
                        |> Network.updateAddress addressId (s_hasTags False)
                        |> Network.updateAddress addressId (\t -> { t | hasClusterTagsOnly = True })

                HasTags _ ->
                    m.network
                        |> Network.updateAddress addressId (s_hasTags False)
                        |> Network.updateAddress addressId (\t -> { t | hasClusterTagsOnly = False })

                _ ->
                    m.network
                        |> Network.updateAddress addressId (s_hasTags False)
                        |> Network.updateAddress addressId (\t -> { t | hasClusterTagsOnly = False })
    in
    tag |> Maybe.map (\n -> { m | network = net n }) |> Maybe.withDefault m


getNextTxEffects : Network -> Id -> Direction -> { addBetweenLinks : Bool, addAnyLinks : Bool } -> Maybe Id -> List Effect
getNextTxEffects network addressId direction { addBetweenLinks, addAnyLinks } neighborId =
    Network.getRecentTxForAddress network (Direction.flip direction) addressId
        |> Maybe.map
            (\tx ->
                if addAnyLinks then
                    case tx.type_ of
                        Tx.Account t ->
                            let
                                config =
                                    { addressId = addressId
                                    , direction = direction
                                    }
                            in
                            WorkflowNextTxByTime.startByHeight config t.raw.height t.raw.currency
                                |> Workflow.mapEffect (WorkflowNextTxByTime config neighborId)
                                |> Workflow.next
                                |> List.map ApiEffect

                        Tx.Utxo t ->
                            let
                                config =
                                    { addressId = addressId
                                    , direction = direction
                                    , allowMultiple = False
                                    }
                            in
                            WorkflowNextUtxoTx.start config t.raw
                                |> Workflow.mapEffect (WorkflowNextUtxoTx config neighborId)
                                |> Workflow.next
                                |> List.map ApiEffect

                else
                    []
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                let
                    config =
                        { addressId = addressId
                        , direction = direction
                        }
                in
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

    else if model.route == route then
        n model

    else
        model
            |> s_route route
            |> (if route == Route.Root then
                    n

                else
                    forcePushHistory
               )
            |> and (updateByRoute_ plugins uc route)
            |> and (syncSidePanel uc)


addPathsToGraph : Plugins -> Update.Config -> Model -> String -> { x | outgoing : Bool, autolinkInTraceMode : Bool } -> List (List PathHopType) -> ( Model, List Effect )
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


addPathToGraph : Plugins -> Update.Config -> Model -> String -> { x | outgoing : Bool, autolinkInTraceMode : Bool } -> List PathHopType -> ( Model, List Effect )
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
            Network.getYForPathFromX model.network { isOutgoing = config.outgoing } startCoordsPrel.x startCoordsPrel.y

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
                            loadAddressWithPosition plugins config.autolinkInTraceMode (Fixed x_ y_) ( net, Data.normalizeIdentifier net adr )

                        Route.TxHop h ->
                            loadTxWithPosition (Fixed x_ y_) config.autolinkInTraceMode False plugins ( net, h )

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
    let
        -- Compute viewport center in graph coordinates for placing new nodes
        viewportCenter =
            Transform.getCurrent model.transform
                |> (\t -> AtViewportCenter (t.x / unit) (t.y / unit))
    in
    case route |> Log.log "route" of
        Route.Root ->
            unselect model

        Route.Network network (Route.Address a _) ->
            let
                id =
                    Id.init network a
            in
            { model | network = Network.clearSelection model.network }
                |> loadAddressWithPosition plugins True viewportCenter id
                |> and (selectAddress id)

        Route.Network network (Route.Tx a) ->
            let
                id =
                    Id.init network a
            in
            { model | network = Network.clearSelection model.network }
                |> loadTxWithPosition viewportCenter True True plugins id
                |> and (selectTx id)

        Route.Network network (Route.Relation a b) ->
            let
                aId =
                    Id.init network a

                bId =
                    Id.init network b

                edgeId =
                    AggEdge.initId aId bId
            in
            { model | network = Network.clearSelection model.network }
                |> loadAddressWithPosition plugins True viewportCenter aId
                |> and (loadAddressWithPosition plugins True viewportCenter bId)
                |> and (selectAggEdge uc edgeId)
                |> and (setTracingMode AggregateTracingMode)

        Route.Path net list ->
            addPathToGraph plugins uc model net { outgoing = True, autolinkInTraceMode = True } list

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
                        addPathsToGraph plugins uc mo net { outgoing = True, autolinkInTraceMode = False } paths
                            |> Tuple.mapSecond ((++) eff)

                    PluginInterface.OutMsgsPathfinder (PluginInterface.ShowPathsInPathfinderWithConfig net c paths) ->
                        addPathsToGraph plugins uc mo net { outgoing = c.outgoing, autolinkInTraceMode = False } paths
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
    , BrowserGotTx
        { pos = pos
        , loadAddresses = loadAddresses
        , autoLinkInTraceMode = autoLinkInTraceMode
        , requestedTxHash = Id.id id
        }
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


focusNeighborAddress : Update.Config -> Id -> Direction -> Model -> ( Model, List Effect )
focusNeighborAddress uc anchorId direction model =
    let
        -- Collect every graph-loaded address on the tx's `direction` side
        -- rather than the single "biggest" candidate from
        -- getAddressForDirection. That heuristic can pick an address that
        -- isn't on the graph (e.g. the biggest non-change output of a UTXO
        -- tx), which would make navigation skip over the neighbor we came
        -- from and get stuck.
        neighborId =
            Network.getTxsForAddress model.network direction anchorId
                |> List.concatMap Tx.listAddressesForTx
                |> List.filterMap
                    (\( d, addr ) ->
                        if d == direction && addr.id /= anchorId then
                            Just addr.id

                        else
                            Nothing
                    )
                |> List.head
    in
    case neighborId |> Maybe.andThen (\nid -> Dict.get nid model.network.addresses) of
        Just neighbor ->
            let
                ( m1, eff ) =
                    selectAddress neighbor.id model

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
                        { x = neighbor.x * unit
                        , y = A.getTo neighbor.y * unit
                        , z = Transform.initZ
                        }
                        m1.transform
            in
            ( { m1 | transform = transform }, eff )

        Nothing ->
            n model


selectAddress : Id -> Model -> ( Model, List Effect )
selectAddress id model =
    if model.selection == SelectedAddress id then
        n model

    else
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


unselectAddress : Id -> Network -> Network
unselectAddress a nw =
    Network.updateAddress a (s_selected False) nw


unhoverAddress : Id -> Network -> Network
unhoverAddress a nw =
    nw.addresses
        |> Dict.get a
        |> Maybe.andThen Address.getClusterId
        |> Maybe.map (\e -> Network.updateAddressesByClusterId e (s_clusterSiblingHovered False) nw)
        |> Maybe.withDefault nw


unselect : Model -> ( Model, List Effect )
unselect model =
    let
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
    , []
    )


unhover : Model -> Model
unhover model =
    let
        network =
            case model.hovered of
                HoveredAddress a ->
                    unhoverAddress a model.network

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


pushHistory : Plugins -> Msg -> Model -> ( Model, List Effect )
pushHistory plugins msg model =
    if History.shallPushHistory plugins msg model then
        forcePushHistory model

    else
        n model


forcePushHistory : Model -> ( Model, List Effect )
forcePushHistory model =
    let
        newHistory =
            makeHistoryEntry model
                |> History.push model.history

        isDirty =
            newHistory /= model.history
    in
    { model
        | history = newHistory
    }
        |> setDirty isDirty


setDirty : Bool -> Model -> ( Model, List Effect )
setDirty isDirty model =
    let
        isD =
            model.isDirty
                || isDirty
    in
    ( { model
        | isDirty = isD
      }
    , if isD then
        Ports.setDirty isD
            |> CmdEffect
            |> List.singleton

      else
        []
    )


setClean : Model -> ( Model, List Effect )
setClean model =
    ( { model | isDirty = False }
    , [ Ports.setDirty False
            |> CmdEffect
      ]
    )


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
        |> flip pair
            [ Route.Root
                |> NavPushRouteEffect
            ]


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

        Just NoTags ->
            True

        _ ->
            False


fetchTagSummaryForIds : Bool -> Dict Id HavingTags -> (Bool -> List ( Id, Api.Data.TagSummary ) -> Msg) -> String -> List String -> List Effect
fetchTagSummaryForIds includeBestClusterTag existing toMsg network ids =
    let
        idsToLoad =
            ids
                |> List.map (Id.init network)
                |> List.filter (isTagSummaryLoaded includeBestClusterTag existing >> not)
    in
    if List.isEmpty idsToLoad then
        []

    else
        [ toMsg includeBestClusterTag
            |> Api.BulkGetAddressTagSummaryEffect { currency = network, addresses = idsToLoad |> List.map Id.id, includeBestClusterTag = includeBestClusterTag }
            |> ApiEffect
        ]


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


addTagSummaryToModel : Bool -> Id -> Api.Data.TagSummary -> Model -> ( Model, List Effect )
addTagSummaryToModel includesBestClusterTag id data m =
    let
        hasClusterTagSummaryData =
            (data.tagCountIndirect |> Maybe.withDefault 0)
                > 0
                || (not <| Dict.isEmpty data.labelSummary)
                || (not <| Dict.isEmpty data.conceptTagCloud)
                || (data.bestLabel /= Nothing)
                || (data.bestActor /= Nothing)

        d =
            if data.tagCount > 0 && includesBestClusterTag then
                HasTagSummaryWithCluster data

            else if data.tagCount > 0 && not includesBestClusterTag then
                HasTagSummaryWithoutCluster data

            else if data.tagCount == 0 && includesBestClusterTag && hasClusterTagSummaryData then
                HasTagSummaryOnlyWithCluster data

            else if data.tagCount == 0 && not includesBestClusterTag then
                NoTagsWithoutCluster

            else
                NoTags

        clusterTagsProbeEffect =
            case d of
                NoTags ->
                    if includesBestClusterTag then
                        m.network.addresses
                            |> Dict.get id
                            |> Maybe.andThen (.data >> RemoteData.toMaybe)
                            |> Maybe.map
                                (.entity
                                    >> (\entityId ->
                                            Api.GetEntityAddressTagsEffect
                                                { currency = Id.network id
                                                , entity = entityId
                                                , pagesize = 1
                                                , nextpage = Nothing
                                                }
                                                (\tags ->
                                                    BrowserGotClusterTagsProbe id (not (List.isEmpty tags.addressTags))
                                                )
                                                |> ApiEffect
                                       )
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []

                    else
                        []

                _ ->
                    []
    in
    ( { m
        | tagSummaries = upsertTagSummary id d m.tagSummaries
      }
        |> updateTagDataOnAddress id
    , clusterTagsProbeEffect
        ++ (data.bestActor |> Maybe.map (List.singleton >> flip fetchActors m.actors) |> Maybe.withDefault [])
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


getBiggestIOBy : (Api.Data.TxValue -> Bool) -> Maybe (List Api.Data.TxValue) -> Set String -> Maybe String
getBiggestIOBy includeIo io exceptAddresses =
    io
        |> Maybe.withDefault []
        |> List.filter includeIo
        |> List.filter (\x -> x.address |> Set.fromList |> Set.intersect exceptAddresses |> Set.isEmpty)
        |> List.sortBy (.value >> .value)
        |> List.reverse
        |> List.head
        |> Maybe.map .address
        |> Maybe.andThen List.head


getBiggestIO : Maybe (List Api.Data.TxValue) -> Set String -> Maybe String
getBiggestIO =
    getBiggestIOBy (always True)


isConsensusChangeOutput : List Api.Data.ConsensusEntry -> Api.Data.TxValue -> Bool
isConsensusChangeOutput consensusEntries output =
    let
        byAddress =
            List.Extra.find (\entry -> List.member entry.output.address output.address) consensusEntries
    in
    case output.index of
        Just outputIndex ->
            case List.Extra.find (\entry -> entry.output.index == outputIndex) consensusEntries of
                Just _ ->
                    True

                Nothing ->
                    byAddress /= Nothing

        Nothing ->
            byAddress /= Nothing


getBiggestNonChangeOutput : Api.Data.TxUtxo -> Set String -> Maybe String
getBiggestNonChangeOutput raw exceptAddresses =
    let
        consensusEntries =
            raw.heuristics
                |> Maybe.andThen .changeHeuristics
                |> Maybe.map .consensus
                |> Maybe.withDefault []
    in
    getBiggestIOBy (isConsensusChangeOutput consensusEntries >> not) raw.outputs exceptAddresses
        |> Maybe.Extra.orElseLazy (\_ -> getBiggestIO raw.outputs exceptAddresses)


getAddressForDirection : Tx -> Direction -> Set String -> Maybe Id
getAddressForDirection tx direction exceptAddress =
    case tx.type_ of
        Tx.Utxo { raw } ->
            (case direction of
                Incoming ->
                    getBiggestIO raw.inputs exceptAddress

                Outgoing ->
                    getBiggestNonChangeOutput raw exceptAddress
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
    Dict.get id model.network.addresses
        |> Maybe.map
            (\addr ->
                let
                    nw2 =
                        let
                            clusterId =
                                Address.getClusterId addr

                            onlyTwoClusterSiblingsOngraph =
                                clusterId
                                    |> Maybe.map (flip Network.getAddressIdsInCluster model.network)
                                    |> Maybe.map (List.length >> (==) 2)
                                    |> Maybe.withDefault False
                        in
                        clusterId
                            |> Maybe.map
                                (\clid ->
                                    if onlyTwoClusterSiblingsOngraph then
                                        Network.updateAddressesByClusterId clid (s_clusterColor Nothing) model.network

                                    else
                                        model.network
                                )
                            |> Maybe.withDefault model.network
                in
                { model
                    | network =
                        unhoverAddress id nw2
                            |> Network.deleteAddress id
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
            )
        |> Maybe.withDefault (n model)


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
    , []
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
    , []
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
                , avoidOverlapingNodes = Just model.config.avoidOverlapingNodes
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
    let
        ( currency, txHash ) =
            case tx.type_ of
                Tx.Account atx ->
                    ( atx.raw.network, atx.raw.identifier )

                Tx.Utxo utxoTx ->
                    ( utxoTx.raw.currency, utxoTx.raw.txHash )
    in
    ( model
    , BrowserGotConversions tx
        |> Api.GetConversionEffect
            { currency = currency
            , txHash = txHash
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
            and (loadAddressWithPosition plugins autoLinkInTraceMode (NextTo ( d, tx.id )) addressId)

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

                        ( HasClusterTagsOnlyButNoDirect, new ) ->
                            new

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
            (\_ -> loadTx False (addressId /= Nothing) plugins txId model)


addMarginPathfinder : BBox -> BBox
addMarginPathfinder bbox =
    { x = bbox.x - unit
    , y = bbox.y - unit * 3
    , width = bbox.width + (2 * unit)
    , height = bbox.height + (8 * unit)
    }


addMarginForExport : BBox -> BBox
addMarginForExport bb =
    let
        relMargin =
            0.0

        absMargin =
            20
    in
    { x = bb.x - absMargin - bb.width * relMargin
    , y = bb.y - absMargin - bb.height * relMargin
    , width = bb.width + absMargin * 2 + bb.width * relMargin * 2
    , height = bb.height + absMargin * 2 + bb.height * relMargin * 2
    }


bboxWithUnit : BBox -> BBox
bboxWithUnit bbox =
    { x = bbox.x * unit
    , y = bbox.y * unit
    , width = bbox.width * unit
    , height = bbox.height * unit
    }


getTagsForExport : Id -> TransactionTable.Model -> ( List Api.Data.TxAccount, Maybe String ) -> Model -> ( Model, List Effect )
getTagsForExport addressId table data model =
    let
        toMsg =
            \includesBestClusterTag result ->
                AddressDetails.BrowserGotBulkTagsForExport table data includesBestClusterTag result
                    |> AddressDetailsMsg addressId
    in
    ( model
    , data
        |> first
        |> List.concatMap (\tx -> [ tx.fromAddress, tx.toAddress ])
        |> Set.fromList
        |> Set.toList
        |> fetchTagSummaryForIds True model.tagSummaries toMsg (Id.network addressId)
    )


{-| Generate the graph transactions CSV export with current tag summaries
-}
generateGraphTxsExport : Update.Config -> Dialog.ExportArea -> Model -> ( Model, List Effect )
generateGraphTxsExport uc exportSelection model =
    let
        config =
            makeGraphTxsExportCSVConfig uc model.tagSummaries

        txAccounts =
            getTxsByExportSelection uc exportSelection model
                |> List.concatMap (explodeTxToAccounts uc.locale)

        ( exportCSV, eff ) =
            ExportCSV.gotData uc config ( txAccounts, Nothing ) model.exportCSVGraph
    in
    ( { model | exportCSVGraph = exportCSV }, eff )


getTxsByExportSelection : Update.Config -> Dialog.ExportArea -> Model -> List Tx
getTxsByExportSelection uc area model =
    case area of
        Dialog.ExportAreaWhole ->
            Dict.values model.network.txs

        Dialog.ExportAreaVisible ->
            uc.size
                |> Maybe.map (getVisibleTxs model)
                |> Maybe.withDefault []

        Dialog.ExportAreaSelected ->
            getSelectedTxs model


{-| Config for exporting all graph transactions as CSV
-}
makeGraphTxsExportCSVConfig : Update.Config -> Dict Id HavingTags -> ExportCSV.Config Api.Data.TxAccount Effect
makeGraphTxsExportCSVConfig uc tagSummaries =
    ExportCSV.config
        { filename =
            Locale.string uc.locale "graph_transactions"
        , toCsv =
            txAccountToCsvRow uc.locale tagSummaries
                >> Maybe.map (List.map (mapFirst (Locale.string uc.locale)))
        , numberOfRows = 10000
        , fetch = \_ -> CmdEffect Cmd.none
        , cmdToEff = Cmd.map (always NoOp) >> CmdEffect
        , notificationToEff = ShowNotificationEffect
        }
        |> ExportCSV.onCompleted (InternalEffect InternalExportGraphTxsCompleted)


{-| Convert a TxAccount to CSV row format, matching AddressDetails.prepareCSV format
-}
txAccountToCsvRow : Locale.Model -> Dict Id HavingTags -> Api.Data.TxAccount -> Maybe (List ( String, String ))
txAccountToCsvRow locModel tagSummaries raw =
    let
        network =
            raw.network

        tagSender =
            Id.init raw.currency raw.fromAddress
                |> flip Dict.get tagSummaries
                |> Maybe.andThen getTagsummary

        tagReceiver =
            Id.init raw.currency raw.toAddress
                |> flip Dict.get tagSummaries
                |> Maybe.andThen getTagsummary
    in
    Just
        (( "Tx_hash", Util.Csv.string raw.txHash )
            :: (if Data.isAccountLike network then
                    [ ( "Token_tx_id"
                      , raw.tokenTxId
                            |> Maybe.map Util.Csv.int
                            |> Maybe.withDefault (Util.Csv.string "")
                      )
                    ]

                else
                    []
               )
            ++ Util.Csv.valuesWithBaseCurrencyFloat "Value"
                raw.value
                locModel
                { network = network
                , asset = raw.currency
                }
            ++ [ ( "Currency", Util.Csv.string <| String.toUpper raw.currency )
               , ( "Height", Util.Csv.int raw.height )
               , ( "Timestamp_utc", Locale.timestampNormal { locModel | zone = Time.utc } <| Data.timestampToPosix raw.timestamp )
               , ( "Sending_address", Util.Csv.string raw.fromAddress )
               , ( "Sending_address_label"
                 , tagSender
                    |> Maybe.andThen .bestActor
                    |> Maybe.withDefault ""
                    |> Util.Csv.string
                 )
               , ( "Receiving_address", Util.Csv.string raw.toAddress )
               , ( "Receiving_address_label"
                 , tagReceiver
                    |> Maybe.andThen .bestActor
                    |> Maybe.withDefault ""
                    |> Util.Csv.string
                 )
               ]
        )


{-| Explode graph Tx into list of TxAccount records (one per input/output for UTXO)
-}
explodeTxToAccounts : Locale.Model -> Tx -> List Api.Data.TxAccount
explodeTxToAccounts locale tx =
    case tx.type_ of
        Tx.Account accountTx ->
            let
                raw =
                    accountTx.raw

                feeRow =
                    raw.fee
                        |> Maybe.andThen
                            (\fee ->
                                if fee.value /= 0 then
                                    Just
                                        { raw
                                            | value = Data.negateValues fee
                                            , toAddress = Locale.string locale "fee"
                                        }

                                else
                                    Nothing
                            )
            in
            raw :: (feeRow |> Maybe.map List.singleton |> Maybe.withDefault [])

        Tx.Utxo utxoTx ->
            Tx.utxoTxToAccountTxs (Just locale) utxoTx


getToAndFromAddresses : Update.Config -> List Tx -> List ( String, String )
getToAndFromAddresses uc =
    let
        feeAddress =
            Locale.string uc.locale "fee"
    in
    List.concatMap (explodeTxToAccounts uc.locale)
        >> List.concatMap (\tx -> [ ( tx.currency, tx.fromAddress ), ( tx.currency, tx.toAddress ) ])
        >> List.filter (\( _, addr ) -> not (String.isEmpty addr) && addr /= feeAddress)


updateByExportMsg : Update.Config -> ExportDialog.Msg -> Dialog.ExportConfig msg -> Model -> ( Model, List Effect )
updateByExportMsg uc msg conf model =
    case msg of
        ExportDialog.UserClickedExport ->
            case conf.fileFormat of
                Dialog.ExportFormatCSV ->
                    exportGraphTxs uc conf model

                Dialog.ExportFormatPDF ->
                    exportGraphImage uc conf model

                Dialog.ExportFormatPNG ->
                    exportGraphImage uc conf model

        ExportDialog.BrowserRenderedGraphForExport ->
            model.config
                |> s_hideForExport NoExport
                |> flip s_config model
                |> n

        ExportDialog.BrowserSentBBox bbox ->
            exportGraph conf bbox model

        ExportDialog.BrowserSentExportGraphResult error ->
            let
                type_ =
                    Dialog.exportFormatToString conf.fileFormat
            in
            ( { model | exportImage = Nothing }
            , error
                |> Maybe.map
                    (Notification.errorDefault
                        >> Notification.map (s_title (Just "An error occurred"))
                    )
                |> Maybe.withDefault
                    (Notification.successDefault "check download folder"
                        |> Notification.map (s_title (Just <| "generating " ++ type_ ++ " success"))
                        |> Notification.map (s_isEphemeral True)
                        |> Notification.map (s_showClose False)
                        |> Notification.map (s_removeDelayMs 4000.0)
                    )
                |> ShowNotificationEffect
                |> List.singleton
            )

        _ ->
            n model


exportGraphTxs : Update.Config -> Dialog.ExportConfig msg -> Model -> ( Model, List Effect )
exportGraphTxs uc conf model =
    -- First collect all addresses from transactions and check if we need to fetch tag summaries
    let
        allAddresses =
            getTxsByExportSelection uc conf.area model
                |> getToAndFromAddresses uc

        -- Group addresses by network for bulk fetching
        addressesByNetwork =
            allAddresses
                |> List.foldl
                    (\( network, addr ) acc ->
                        Dict.update network
                            (\existing ->
                                case existing of
                                    Nothing ->
                                        Just (Set.singleton addr)

                                    Just addrs ->
                                        Just (Set.insert addr addrs)
                            )
                            acc
                    )
                    Dict.empty

        -- Find addresses missing tag summaries
        missingByNetwork =
            addressesByNetwork
                |> Dict.toList
                |> List.filterMap
                    (\( network, addrs ) ->
                        let
                            missing =
                                addrs
                                    |> Set.toList
                                    |> List.filter
                                        (\addr ->
                                            Id.init network addr
                                                |> isTagSummaryLoaded True model.tagSummaries
                                                |> not
                                        )
                        in
                        if List.isEmpty missing then
                            Nothing

                        else
                            Just ( network, missing )
                    )

        config =
            makeGraphTxsExportCSVConfig uc model.tagSummaries

        ( newModel, _ ) =
            ExportCSV.update (ExportCSV.BrowserGotTime conf.time) config model.exportCSVGraph
                |> mapFirst (flip s_exportCSVGraph model)
    in
    if List.isEmpty missingByNetwork then
        -- All tag summaries already loaded, proceed with export
        generateGraphTxsExport uc conf.area newModel

    else
        -- Need to fetch missing tag summaries first
        let
            toMsg =
                BrowserGotTagSummariesForExportGraphTxsAsCSV conf.area

            fetchEffects =
                missingByNetwork
                    |> List.concatMap
                        (\( network, addrs ) ->
                            fetchTagSummaryForIds True model.tagSummaries toMsg network addrs
                        )
        in
        ( newModel, fetchEffects )


exportGraphImage : Update.Config -> Dialog.ExportConfig msg -> Model -> ( Model, List Effect )
exportGraphImage _ conf model =
    let
        selector =
            case conf.area of
                Dialog.ExportAreaSelected ->
                    Just "g[data-selected=true]"

                Dialog.ExportAreaWhole ->
                    Just "g[data-selected]"

                Dialog.ExportAreaVisible ->
                    Nothing
    in
    { model
        | exportImage =
            PrepareImageForExport
                |> Just
    }
        |> n
        |> and
            (case selector of
                Just sel ->
                    \mo ->
                        Ports.getBBox ( "svg#" ++ graphId, sel )
                            |> CmdEffect
                            |> List.singleton
                            |> pair mo

                Nothing ->
                    exportGraph conf Nothing
            )



-- Helper function for transaction hover logic


handleTxHover : Id -> Model -> ( Model, List Effect )
handleTxHover id model =
    if model.hovered == HoveredTx id then
        n model

    else
        let
            hovered _ =
                let
                    unhovered =
                        unhover model
                in
                { unhovered
                    | network =
                        Network.updateTx id (s_hovered True) unhovered.network
                            |> Network.trySetHoverConversionLoop id True
                    , hovered = HoveredTx id
                }
        in
        case model.details of
            Just (TxDetails txid _) ->
                if id /= txid then
                    hovered () |> n

                else
                    n model

            _ ->
                hovered () |> n
