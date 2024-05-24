module Update.Pathfinder exposing (update, updateByRoute)

import Api.Data
import Config.Update as Update
import Dict
import Dict.Nonempty as NDict
import DurationDatePicker
import Effect exposing (and, n)
import Effect.Api as Api
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Log
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (Coords, relativeToGraphZero)
import Model.Graph.History as History
import Model.Graph.Table as GT
import Model.Graph.Transform as Transform
import Model.Locale exposing (State(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address as Address
import Model.Pathfinder.DatePicker exposing (userDefinedRangeDatePickerSettings)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network
import Model.Pathfinder.Table as PT
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Model.Pathfinder.Tx as Tx
import Model.Search as Search
import Msg.Pathfinder as Msg exposing (AddressDetailsMsg(..), DisplaySettingsMsg(..), Msg(..), TxDetailsMsg(..))
import Msg.Search as Search
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Result.Extra
import Route.Pathfinder as Route
import Tuple exposing (first, mapFirst, pair, second)
import Tuple2 exposing (pairTo)
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Table exposing (UpdateSearchTerm(..), appendData)
import Update.Graph.Transform as Transform
import Update.Pathfinder.Network as Network
import Update.Search as Search
import Util.Pathfinder exposing (getAddress)
import Util.Pathfinder.History as History


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory msg
        |> updateByMsg plugins uc msg


resultLineToRoute : Search.ResultLine -> Route.Route
resultLineToRoute search =
    case search of
        Search.Address net address ->
            Route.Network net (Route.Address address)

        Search.Tx net h ->
            Route.Network net (Route.Tx h)

        Search.Block net b ->
            Route.Network net (Route.Block b)

        Search.Label s ->
            Route.Label s

        Search.Actor ( id, name ) ->
            Route.Actor id


updateByMsg : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
updateByMsg plugins uc msg model =
    case Log.truncate "msg" msg of
        PluginMsg _ ->
            -- handled in src/Update.elm
            n model

        NoOp ->
            n model

        BrowserGotActor id data ->
            n { model | actors = Dict.insert id data model.actors }

        UserPressedCtrlKey ->
            let
                vs =
                    model.view
            in
            ( model |> s_view { vs | pointerTool = Select }, [] )

        UserReleasedCtrlKey ->
            let
                vs =
                    model.view
            in
            ( model |> s_view { vs | pointerTool = Drag }, [] )

        BrowserGotAddressData id data ->
            model
                |> s_network (Network.updateAddress id (s_data (Success data)) model.network)
                |> pairTo (fetchActorsForAddress data model.actors)

        BrowserGotRecentTx id direction data ->
            let
                getHash tx =
                    case tx of
                        Api.Data.AddressTxAddressTxUtxo t ->
                            t.txHash

                        Api.Data.AddressTxTxAccount t ->
                            t.txHash
            in
            ( model
            , data.addressTxs
                |> List.head
                |> Maybe.map getHash
                |> Maybe.map
                    (\txHash ->
                        BrowserGotTxForAddress id direction
                            |> Api.GetTxEffect
                                { currency = Id.network id
                                , txHash = txHash
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                            |> ApiEffect
                            |> List.singleton
                    )
                |> Maybe.withDefault []
            )

        BrowserGotTxForAddress id direction data ->
            browserGotTxForAddress plugins uc id direction data model

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
            n (closeDetailsView model)

        TxDetailsMsg submsg ->
            case model.view.detailsViewState of
                TxDetails id txViewState ->
                    let
                        nVs =
                            case submsg of
                                UserClickedToggleIOTable ->
                                    { txViewState | ioTableOpen = not txViewState.ioTableOpen }
                    in
                    ( (setViewState <| s_detailsViewState (TxDetails id nVs)) model, [] )

                _ ->
                    n model

        AddressDetailsMsg subm ->
            case model.view.detailsViewState of
                AddressDetails id ad ->
                    let
                        ( addressViewDetails, e ) =
                            case subm of
                                UserClickedToggleNeighborsTable ->
                                    let
                                        tables =
                                            [ ( ad.neighborsIncoming, Incoming ), ( ad.neighborsOutgoing, Outgoing ) ]

                                        fetchFirstPageFn =
                                            \( tbl, dir ) ->
                                                if List.isEmpty tbl.t.data then
                                                    Just
                                                        ((GotNeighborsForAddressDetails id dir >> AddressDetailsMsg)
                                                            |> Api.GetAddressNeighborsEffect
                                                                { currency = Id.network id
                                                                , address = Id.id id
                                                                , includeLabels = True
                                                                , onlyIds = Nothing
                                                                , isOutgoing = dir == Outgoing
                                                                , pagesize = tbl.itemsPerPage
                                                                , nextpage = tbl.t.nextpage
                                                                }
                                                            |> ApiEffect
                                                        )

                                                else
                                                    Nothing

                                        eff =
                                            List.filterMap fetchFirstPageFn tables
                                    in
                                    ( { ad | neighborsTableOpen = not ad.neighborsTableOpen }, eff )

                                UserClickedNextPageNeighborsTable dir ->
                                    let
                                        ( tbl, setter ) =
                                            case dir of
                                                Incoming ->
                                                    ( ad.neighborsIncoming, s_neighborsIncoming )

                                                Outgoing ->
                                                    ( ad.neighborsOutgoing, s_neighborsOutgoing )

                                        ( eff, loading ) =
                                            if not (tbl.t.nextpage == Nothing) then
                                                ( (GotNeighborsForAddressDetails id dir >> AddressDetailsMsg)
                                                    |> Api.GetAddressNeighborsEffect
                                                        { currency = Id.network id
                                                        , address = Id.id id
                                                        , includeLabels = True
                                                        , onlyIds = Nothing
                                                        , isOutgoing = dir == Outgoing
                                                        , pagesize = tbl.itemsPerPage
                                                        , nextpage = tbl.t.nextpage
                                                        }
                                                    |> ApiEffect
                                                    |> List.singleton
                                                , True
                                                )

                                            else
                                                ( [], False )
                                    in
                                    if loading then
                                        ( ad |> setter ((PT.incPage >> PT.setLoading loading) tbl), eff )

                                    else
                                        n ad

                                UserClickedPreviousPageNeighborsTable dir ->
                                    let
                                        ( tbl, setter ) =
                                            case dir of
                                                Incoming ->
                                                    ( ad.neighborsIncoming, s_neighborsIncoming )

                                                Outgoing ->
                                                    ( ad.neighborsOutgoing, s_neighborsOutgoing )
                                    in
                                    ( ad |> setter (PT.decPage tbl), [] )

                                GotNeighborsForAddressDetails requestId dir neighbors ->
                                    if requestId == id then
                                        n
                                            (case dir of
                                                Incoming ->
                                                    { ad
                                                        | neighborsIncoming = appendPagedTableData ad.neighborsIncoming NeighborsTable.filter neighbors.nextPage neighbors.neighbors
                                                    }

                                                Outgoing ->
                                                    { ad
                                                        | neighborsOutgoing = appendPagedTableData ad.neighborsOutgoing NeighborsTable.filter neighbors.nextPage neighbors.neighbors
                                                    }
                                            )

                                    else
                                        n ad

                                UserClickedNextPageTransactionTable ->
                                    let
                                        ( eff, loading ) =
                                            if not (ad.txs.t.nextpage == Nothing) then
                                                ( (GotTxsForAddressDetails id >> AddressDetailsMsg)
                                                    |> Api.GetAddressTxsEffect
                                                        { currency = Id.network id
                                                        , address = Id.id id
                                                        , direction = Nothing
                                                        , pagesize = ad.txs.itemsPerPage
                                                        , nextpage = ad.txs.t.nextpage
                                                        , order = Nothing
                                                        , minHeight = Nothing
                                                        , maxHeight = Nothing
                                                        }
                                                    |> ApiEffect
                                                    |> List.singleton
                                                , True
                                                )

                                            else
                                                ( [], False )
                                    in
                                    if loading then
                                        ( { ad | txs = (PT.incPage >> PT.setLoading loading) ad.txs }, eff )

                                    else
                                        n ad

                                UserClickedPreviousPageTransactionTable ->
                                    ( { ad | txs = PT.decPage ad.txs }, [] )

                                UserClickedToggleTransactionTable ->
                                    let
                                        eff =
                                            if List.isEmpty ad.txs.t.data then
                                                (GotTxsForAddressDetails id >> AddressDetailsMsg)
                                                    |> Api.GetAddressTxsEffect
                                                        { currency = Id.network id
                                                        , address = Id.id id
                                                        , direction = Nothing
                                                        , pagesize = ad.txs.itemsPerPage
                                                        , nextpage = ad.txs.t.nextpage
                                                        , order = Nothing
                                                        , minHeight = Nothing
                                                        , maxHeight = Nothing
                                                        }
                                                    |> ApiEffect
                                                    |> List.singleton

                                            else
                                                []
                                    in
                                    ( { ad | transactionsTableOpen = not ad.transactionsTableOpen }, eff )

                                GotTxsForAddressDetails responseId txs ->
                                    if responseId == id then
                                        n
                                            { ad
                                                | txs = appendPagedTableData ad.txs TransactionTable.filter txs.nextPage txs.addressTxs
                                            }

                                    else
                                        n ad
                    in
                    ( (setViewState <| s_detailsViewState (AddressDetails id addressViewDetails)) model, e )

                TxDetails _ _ ->
                    n model

                NoDetails ->
                    n model

        UserClickedRestart ->
            -- Handled upstream
            n model

        --n Init.Pathfinder.init
        -- TODO: Implement
        UserClickedUndo ->
            n model

        UserClickedRedo ->
            n model

        UserClickedHighlighter ->
            n model

        UserClickedImportFile ->
            n model

        UserClickedExportGraph ->
            n model

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
            in
            if click then
                ( model
                , Route.Root
                    |> NavPushRouteEffect
                    |> List.singleton
                )

            else
                n model

        UserReleasesMouseButton ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging _ start coords ->
                    n
                        { model
                            | dragging = NoDragging
                        }

                DraggingNode id start coords ->
                    n
                        { model
                            | network = Debug.todo "release node"
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
                                    z
                                    model.transform
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserPushesLeftMouseButtonOnGraph coords ->
            { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            Dragging model.transform (relativeToGraphZero uc.size coords) (relativeToGraphZero uc.size coords)

                        _ ->
                            NoDragging
            }
                |> n

        UserMovesMouseOnGraph coords ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging transform start _ ->
                    (case model.view.pointerTool of
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
                            Transform.vector start (relativeToGraphZero uc.size coords) model.transform
                    in
                    { model
                        | dragging = DraggingNode id start (relativeToGraphZero uc.size coords)
                    }
                        |> n

        AnimationFrameDeltaForTransform delta ->
            n
                { model
                    | transform = Transform.transition delta model.transform
                }

        UserClickedAddressExpandHandle id direction ->
            ( model
            , BrowserGotRecentTx id direction
                |> Api.GetAddressTxsEffect
                    { currency = Id.network id
                    , address = Id.id id
                    , direction = Just direction
                    , pagesize = 1
                    , nextpage = Nothing
                    , order = Nothing
                    , minHeight = Nothing
                    , maxHeight = Nothing
                    }
                |> ApiEffect
                |> List.singleton
            )

        UserClickedAddress id ->
            ( model
            , Route.addressRoute
                { network = Id.network id
                , address = Id.id id
                }
                |> NavPushRouteEffect
                |> List.singleton
            )

        UserClickedAddressCheckboxInTable id ->
            loadAddress plugins id model

        UserClickedTx id ->
            ( model
            , Route.txRoute
                { network = Id.network id
                , txHash = Id.id id
                }
                |> NavPushRouteEffect
                |> List.singleton
            )

        UserClickedTxCheckboxInTable tx ->
            case tx of
                Api.Data.AddressTxTxAccount _ ->
                    n model

                Api.Data.AddressTxAddressTxUtxo t ->
                    ( model
                    , let
                        id =
                            Id.init t.currency t.txHash
                      in
                      BrowserGotTx id
                        |> Api.GetTxEffect
                            { currency = Id.network id
                            , txHash = Id.id id
                            , includeIo = True
                            , tokenTxId = Nothing
                            }
                        |> ApiEffect
                        |> List.singleton
                    )

        BrowserGotTx id tx ->
            let
                nw =
                    Network.addTx id tx model.network
            in
            ( { model | network = nw } |> checkSelection, [] )

        ChangedDisplaySettingsMsg submsg ->
            case submsg of
                ChangePointerTool tool ->
                    let
                        vs =
                            model.view
                    in
                    ( { model | view = { vs | pointerTool = tool } }, [] )

        BrowserGotFromDateBlock dt blockAt ->
            n model

        BrowserGotToDateBlock dt blockAt ->
            n model

        UpdateDateRangePicker subMsg ->
            let
                ( newPicker, maybeRuntime ) =
                    DurationDatePicker.update (userDefinedRangeDatePickerSettings uc.locale model.currentTime) subMsg model.dateRangePicker

                ( startTime, endTime ) =
                    Maybe.map (\( start, end ) -> ( Just start, Just end )) maybeRuntime |> Maybe.withDefault ( model.fromDate, model.toDate )

                eff =
                    case model.view.detailsViewState of
                        AddressDetails id _ ->
                            let
                                startEff =
                                    case startTime of
                                        Just st ->
                                            BrowserGotFromDateBlock st
                                                |> Api.GetBlockByDateEffect
                                                    { currency = Id.network id
                                                    , datetime = st
                                                    }
                                                |> ApiEffect
                                                |> List.singleton

                                        _ ->
                                            []

                                endEff =
                                    case endTime of
                                        Just et ->
                                            BrowserGotToDateBlock et
                                                |> Api.GetBlockByDateEffect
                                                    { currency = Id.network id
                                                    , datetime = et
                                                    }
                                                |> ApiEffect
                                                |> List.singleton

                                        _ ->
                                            []
                            in
                            startEff ++ endEff

                        _ ->
                            []
            in
            case maybeRuntime of
                Just _ ->
                    ( { model | dateRangePicker = newPicker, fromDate = startTime, toDate = endTime }, eff )

                _ ->
                    n { model | dateRangePicker = newPicker }

        OpenDateRangePicker ->
            n { model | dateRangePicker = DurationDatePicker.openPicker (userDefinedRangeDatePickerSettings uc.locale model.currentTime) model.currentTime model.fromDate model.toDate model.dateRangePicker }

        CloseDateRangePicker ->
            n { model | dateRangePicker = DurationDatePicker.closePicker model.dateRangePicker }

        Tick time ->
            n { model | currentTime = time }


updateByRoute : Plugins -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute plugins route model =
    forcePushHistory model
        |> updateByRoute_ plugins route


updateByRoute_ : Plugins -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute_ plugins route model =
    case route |> Log.log "route" of
        Route.Root ->
            n model

        Route.Network network (Route.Address a) ->
            let
                id =
                    Id.init network a
            in
            loadAddress plugins id model
                |> mapFirst (selectAddress id)

        Route.Network network (Route.Tx a) ->
            let
                id =
                    Id.init network a
            in
            loadTx plugins id model
                |> mapFirst (selectTx id)

        _ ->
            n model


loadAddress : Plugins -> Id -> Model -> ( Model, List Effect )
loadAddress _ id model =
    let
        nw =
            Network.addAddress id model.network
                |> Network.updateAddress id (s_data Loading)
    in
    ( { model | network = nw }
    , BrowserGotAddressData id
        |> Api.GetAddressEffect
            { currency = Id.network id
            , address = Id.id id
            }
        |> ApiEffect
        |> List.singleton
    )


loadTx : Plugins -> Id -> Model -> ( Model, List Effect )
loadTx _ id model =
    ( model
    , BrowserGotTx id
        |> Api.GetTxEffect
            { currency = Id.network id
            , txHash = Id.id id
            , includeIo = True
            , tokenTxId = Nothing
            }
        |> ApiEffect
        |> List.singleton
    )


selectTx : Id -> Model -> Model
selectTx id model =
    if Network.hasTx id model.network then
        (s_selection (SelectedTx id)
            >> (setViewState <| s_detailsViewState (TxDetails id getTxDetailsDefaultState))
        )
            model

    else
        s_selection (WillSelectTx id) model


selectAddress : Id -> Model -> Model
selectAddress id model =
    if Network.hasAddress id model.network then
        let
            m1 =
                (s_selection (SelectedAddress id)
                    >> (setViewState <| s_detailsViewState (AddressDetails id (getAddressDetailsViewStateDefaultForAddress id model)))
                )
                    model
        in
        { m1 | network = Network.selectAddress (Network.unSelectAll m1.network) id }

    else
        s_selection (WillSelectAddress id) model


pushHistory : Msg -> Model -> Model
pushHistory msg model =
    if History.shallPushHistory msg model then
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
    { network = model.network
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
                }
            )
        |> Maybe.withDefault model
        |> n


fetchActor : String -> Effect
fetchActor id =
    BrowserGotActor id |> Api.GetActorEffect { actorId = id } |> ApiEffect


fetchActorsForAddress : Api.Data.Address -> Dict.Dict String Api.Data.Actor -> List Effect
fetchActorsForAddress d existing =
    d.actors
        |> Maybe.map (List.filter (\l -> not (Dict.member l.id existing)))
        |> Maybe.map (List.map (.id >> fetchActor))
        |> Maybe.withDefault []


browserGotTxForAddress : Plugins -> Update.Config -> Id -> Direction -> Api.Data.Tx -> Model -> ( Model, List Effect )
browserGotTxForAddress plugins _ id direction tx model =
    let
        newmodel =
            { model
                | network = Network.addTx id tx model.network
            }

        getBiggest io =
            Maybe.withDefault [] io
                |> List.sortBy (.value >> .value)
                |> List.reverse
                |> List.head
                |> Maybe.map .address
                |> Maybe.andThen List.head

        -- TODO what if multisig?
        firstAddress =
            case tx of
                Api.Data.TxTxUtxo t ->
                    case direction of
                        Incoming ->
                            getBiggest t.inputs

                        Outgoing ->
                            getBiggest t.outputs

                Api.Data.TxTxAccount t ->
                    case direction of
                        Incoming ->
                            Just t.fromAddress

                        Outgoing ->
                            Just t.toAddress
    in
    firstAddress
        |> Maybe.map
            (\a ->
                loadAddress plugins (Id.init (Id.network id) a) newmodel
            )
        |> Maybe.withDefault (n newmodel)


appendPagedTableData : PT.PagedTable p -> GT.Filter p -> Maybe String -> List p -> PT.PagedTable p
appendPagedTableData pt f nextPage data =
    { pt
        | t =
            appendData f data pt.t
                |> s_nextpage nextPage
                |> s_loading False
    }


checkSelection : Model -> Model
checkSelection model =
    case model.selection of
        WillSelectTx id ->
            selectTx id model

        WillSelectAddress id ->
            selectAddress id model

        _ ->
            model
