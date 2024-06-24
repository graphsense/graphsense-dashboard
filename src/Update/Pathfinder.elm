module Update.Pathfinder exposing (update, updateByRoute)

import Animation as A
import Api.Data
import Basics.Extra exposing (flip)
import Config.Update as Update
import Dict
import DurationDatePicker
import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Init.Graph.Transform as Transform
import Init.Pathfinder.Details.AddressDetails exposing (getAddressDetailsViewStateDefaultForAddress)
import Init.Pathfinder.Details.TxDetails as TxDetails
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import List.Extra
import Log
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (relativeToGraphZero)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale exposing (State(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address as Address
import Model.Pathfinder.DatePicker exposing (pathfinderRangeDatePickerSettings)
import Model.Pathfinder.Details as Details
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Model.Pathfinder.Tx as Tx
import Model.Search as Search
import Msg.Pathfinder as Msg
    exposing
        ( AddressDetailsMsg(..)
        , DisplaySettingsMsg(..)
        , Msg(..)
        , TxDetailsMsg(..)
        , WorkflowNextTxByTimeMsg(..)
        , WorkflowNextUtxoTxMsg(..)
        )
import Msg.Search as Search
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Route.Pathfinder as Route
import Set
import Svg.Attributes exposing (x)
import Time exposing (Posix)
import Tuple exposing (mapFirst, pair)
import Tuple2 exposing (pairTo)
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Table exposing (UpdateSearchTerm(..))
import Update.Graph.Transform as Transform
import Update.Pathfinder.AddressDetails as AddressDetails
import Update.Pathfinder.Network as Network
import Update.Pathfinder.Node as Node
import Update.Pathfinder.Tx as Tx
import Update.Pathfinder.TxDetails as TxDetails
import Update.Pathfinder.WorkflowNextTxByTime as WorkflowNextTxByTime
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Update.Search as Search
import Util.Pathfinder.History as History


type SetOrNoSet x
    = Set x
    | NoSet
    | Reset


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

        Search.Actor ( id, _ ) ->
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
            let
                network =
                    if List.any (.id >> (==) "exchange") data.categories then
                        Network.updateAddressIf
                            (.data
                                >> RemoteData.toMaybe
                                >> Maybe.andThen .actors
                                >> Maybe.withDefault []
                                >> List.Extra.find (.id >> (==) id)
                                >> (/=) Nothing
                            )
                            (s_isExchange True)
                            model.network

                    else
                        model.network
            in
            n
                { model
                    | actors = Dict.insert id data model.actors
                    , network = network
                }

        UserPressedCtrlKey ->
            n { model | pointerTool = Select }

        UserPressedDeleteKey ->
            case model.selection of
                SelectedAddress id ->
                    removeAddress id model
                _ ->
                    n model

        UserReleasedCtrlKey ->
            n { model | pointerTool = Drag }

        BrowserGotAddressData id data ->
            model
                |> s_network (Network.updateAddress id (s_data (Success data)) model.network)
                |> pairTo (fetchActorsForAddress data model.actors)

        BrowserGotTxForAddress addressId direction data ->
            browserGotTxForAddress plugins uc addressId direction data model

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
            { model | details = Nothing }
                |> n

        TxDetailsMsg submsg ->
            case model.details of
                Just (Details.Tx id txViewState) ->
                    let
                        ( nVs, eff ) =
                            TxDetails.update submsg txViewState
                    in
                    ( { model | details = Just (Details.Tx id nVs) }, eff )

                _ ->
                    n model

        AddressDetailsMsg subm ->
            case model.details of
                Just (Details.Address id ad) ->
                    let
                        ( addressViewDetails, eff ) =
                            AddressDetails.update subm id ad
                    in
                    ( { model | details = Just (Details.Address id addressViewDetails) }, eff )

                _ ->
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

                Dragging _ _ _ ->
                    n
                        { model
                            | dragging = NoDragging
                        }

                DraggingNode id _ _ ->
                    n
                        { model
                            | network =
                                Network.updateAddress id Node.release model.network
                                    |> Network.updateTx id (Tx.updateUtxo Node.release)
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

        UserPushesLeftMouseButtonOnAddress id coords ->
            { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            DraggingNode id coords coords

                        _ ->
                            model.dragging
            }
                |> n

        UserPushesLeftMouseButtonOnUtxoTx id coords ->
            { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            DraggingNode id coords coords

                        _ ->
                            model.dragging
            }
                |> n

        UserMovesMouseOnGraph coords ->
            case model.dragging of
                NoDragging ->
                    n model

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
                    in
                    { model
                        | network =
                            Network.updateAddress id (Node.move vectorRel) model.network
                                |> Network.updateTx id
                                    (Tx.updateUtxo (Node.move vectorRel))
                        , dragging = DraggingNode id start coords
                    }
                        |> n

        AnimationFrameDeltaForTransform delta ->
            n
                { model
                    | transform = Transform.transition delta model.transform
                }

        AnimationFrameDeltaForMove delta ->
            n
                { model
                    | network =
                        Network.animateAddresses delta model.network
                            |> Network.animateTxs delta
                }

        UserClickedAddressExpandHandle id direction ->
            let
                ( newmodel, eff ) =
                    model
                        |> selectAddress id
                        |> openAddressTransactionsTable
            in
            ( newmodel
            , getNextTxEffects newmodel id direction
                ++ eff
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
            if Dict.member id model.network.addresses then
                removeAddress id model

            else
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
                Api.Data.AddressTxTxAccount t ->
                    n
                        { model
                            | network =
                                Network.addTx (Api.Data.TxTxAccount t) model.network
                                    |> Network.addAddress (Id.init t.currency t.fromAddress)
                                    |> Network.addAddress (Id.init t.currency t.toAddress)
                        }

                Api.Data.AddressTxAddressTxUtxo t ->
                    let
                        id =
                            Id.init t.currency t.txHash
                    in
                    if Dict.member id model.network.txs then
                        Network.deleteTx id model.network
                            |> flip s_network model
                            |> n

                    else
                        BrowserGotTx
                            |> Api.GetTxEffect
                                { currency = Id.network id
                                , txHash = Id.id id
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                            |> ApiEffect
                            |> List.singleton
                            |> pair model

        UserClickedRemoveAddressFromGraph id ->
            removeAddress id model

        BrowserGotTx tx ->
            let
                nw =
                    Network.addTx tx model.network
            in
            { model
                | network = nw
            }
                |> checkSelection
                |> n

        ChangedDisplaySettingsMsg submsg ->
            case submsg of
                ChangePointerTool tool ->
                    n { model | pointerTool = tool }

        BrowserGotFromDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange model (Set blockAt.beforeBlock) NoSet

        BrowserGotToDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange model NoSet (Set blockAt.afterBlock)

        UpdateDateRangePicker subMsg ->
            let
                ( mindate, maxdate ) =
                    getMinAndMaxSelectableDateFromModel model

                ( newPicker, maybeRuntime ) =
                    DurationDatePicker.update (pathfinderRangeDatePickerSettings uc.locale mindate maxdate) subMsg model.dateRangePicker

                ( startTime, endTime ) =
                    Maybe.map (\( start, end ) -> ( Just start, Just end )) maybeRuntime |> Maybe.withDefault ( model.fromDate, model.toDate )

                eff =
                    case model.details of
                        Just (Details.Address id _) ->
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
            let
                ( mindate, maxdate ) =
                    getMinAndMaxSelectableDateFromModel model
            in
            n { model | dateRangePicker = DurationDatePicker.openPicker (pathfinderRangeDatePickerSettings uc.locale mindate maxdate) maxdate model.fromDate model.toDate model.dateRangePicker }

        CloseDateRangePicker ->
            n { model | dateRangePicker = DurationDatePicker.closePicker model.dateRangePicker }

        ResetDateRangePicker ->
            let
                ( m2, eff ) =
                    updateDatePickerRangeBlockRange model Reset Reset
            in
            ( { m2 | dateRangePicker = DurationDatePicker.closePicker model.dateRangePicker, fromDate = Nothing, toDate = Nothing }, eff )

        Tick time ->
            n { model | currentTime = time }

        WorkflowNextUtxoTx context wm ->
            WorkflowNextUtxoTx.update context wm model

        WorkflowNextTxByTime context wm ->
            WorkflowNextTxByTime.update context wm model


getNextTxEffects : Model -> Id -> Direction -> List Effect
getNextTxEffects model addressId direction =
    let
        getTxSet =
            case direction of
                Incoming ->
                    .outgoingTxs

                Outgoing ->
                    .incomingTxs

        context =
            { addressId = addressId
            , direction = direction
            }
    in
    Dict.get addressId model.network.addresses
        |> Maybe.map
            (\address ->
                getTxSet address
                    |> Set.toList
                    |> List.filterMap (\txId -> Dict.get txId model.network.txs)
                    |> List.sortBy Tx.getRawTimestamp
                    |> List.Extra.last
                    |> Maybe.map
                        (\tx ->
                            case tx.type_ of
                                Tx.Account t ->
                                    BrowserGotBlockHeight
                                        >> WorkflowNextTxByTime context
                                        |> Api.GetBlockByDateEffect
                                            { currency = t.raw.currency
                                            , datetime =
                                                t.raw.timestamp
                                                    |> (*) 1000
                                                    |> Time.millisToPosix
                                            }

                                Tx.Utxo t ->
                                    let
                                        ( listLinkedTxRefs, getIo ) =
                                            case direction of
                                                Incoming ->
                                                    ( Api.ListSpendingTxRefsEffect, .inputs )

                                                Outgoing ->
                                                    ( Api.ListSpentInTxRefsEffect, .outputs )

                                        index =
                                            getIo t.raw
                                                |> Maybe.andThen
                                                    (List.Extra.findIndex
                                                        (.address >> List.any ((==) (Id.id addressId)))
                                                    )
                                    in
                                    BrowserGotReferencedTxs
                                        >> WorkflowNextUtxoTx context
                                        |> listLinkedTxRefs
                                            { currency = t.raw.currency
                                            , txHash = t.raw.txHash
                                            , index = index
                                            }
                        )
                    |> Maybe.withDefault
                        (BrowserGotRecentTx
                            >> WorkflowNextTxByTime context
                            |> Api.GetAddressTxsEffect
                                { currency = Id.network addressId
                                , address = Id.id addressId
                                , direction = Just direction
                                , pagesize = 1
                                , nextpage = Nothing
                                , order = Nothing
                                , minHeight = Nothing
                                , maxHeight = Nothing
                                }
                        )
                    |> List.singleton
            )
        |> Maybe.withDefault []
        |> List.map ApiEffect


getMinAndMaxSelectableDateFromModel : Model -> ( Posix, Posix )
getMinAndMaxSelectableDateFromModel model =
    let
        default =
            ( Time.millisToPosix 0, model.currentTime )
    in
    case model.details of
        Just (Details.Address id _) ->
            Dict.get id model.network.addresses
                |> Maybe.andThen Address.getActivityRange
                |> Maybe.withDefault default

        _ ->
            default


updateDatePickerRangeBlockRange : Model -> SetOrNoSet Int -> SetOrNoSet Int -> ( Model, List Effect )
updateDatePickerRangeBlockRange model txMinBlock txMaxBlock =
    let
        ( m2, eff ) =
            case model.details of
                Just (Details.Address id ad) ->
                    let
                        txmin =
                            case txMinBlock of
                                Reset ->
                                    Nothing

                                NoSet ->
                                    ad.txMinBlock

                                Set x ->
                                    Just x

                        txmax =
                            case txMaxBlock of
                                Reset ->
                                    Nothing

                                NoSet ->
                                    ad.txMaxBlock

                                Set x ->
                                    Just x

                        effects =
                            case ( txmin, txmax ) of
                                ( Just min, Just max ) ->
                                    (GotTxsForAddressDetails id >> AddressDetailsMsg)
                                        |> Api.GetAddressTxsEffect
                                            { currency = Id.network id
                                            , address = Id.id id
                                            , direction = Nothing
                                            , pagesize = ad.txs.itemsPerPage
                                            , nextpage = Nothing
                                            , order = Nothing
                                            , minHeight = Just min
                                            , maxHeight = Just max
                                            }
                                        |> ApiEffect
                                        |> List.singleton

                                ( Nothing, Nothing ) ->
                                    (GotTxsForAddressDetails id >> AddressDetailsMsg)
                                        |> Api.GetAddressTxsEffect
                                            { currency = Id.network id
                                            , address = Id.id id
                                            , direction = Nothing
                                            , pagesize = ad.txs.itemsPerPage
                                            , nextpage = Nothing
                                            , order = Nothing
                                            , minHeight = Nothing
                                            , maxHeight = Nothing
                                            }
                                        |> ApiEffect
                                        |> List.singleton

                                _ ->
                                    []

                        txsnew =
                            case ( txmin, txmax ) of
                                ( Just _, Just _ ) ->
                                    TransactionTable.init Nothing

                                ( Nothing, Nothing ) ->
                                    TransactionTable.init Nothing

                                _ ->
                                    ad.txs
                    in
                    ( { model
                        | details =
                            Just
                                (Details.Address id
                                    { ad | txMinBlock = txmin, txMaxBlock = txmax, txs = txsnew }
                                )
                      }
                    , effects
                    )

                _ ->
                    n model
    in
    ( m2, eff )


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
    , BrowserGotTx
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
        let
            selectedTx =
                case model.selection of
                    SelectedTx a ->
                        Just a

                    _ ->
                        Nothing

            m1 =
                unselect model
                    |> s_details (Details.Tx id TxDetails.init |> Just)
        in
        selectedTx
            |> Maybe.map (\a -> Network.updateTx a (Tx.updateUtxo (s_selected False)) m1.network)
            |> Maybe.withDefault m1.network
            |> Network.updateTx id (Tx.updateUtxo (s_selected True))
            |> flip s_network m1
            |> s_selection (SelectedTx id)

    else
        s_selection (WillSelectTx id) model


selectAddress : Id -> Model -> Model
selectAddress id model =
    if Network.hasAddress id model.network then
        let
            m1 =
                unselect model
                    |> s_details (Details.Address id (getAddressDetailsViewStateDefaultForAddress id model) |> Just)
        in
        Network.updateAddress id (s_selected True) m1.network
            |> flip s_network m1
            |> s_selection (SelectedAddress id)

    else
        s_selection (WillSelectAddress id) model


unselect : Model -> Model
unselect model =
    let
        network =
            case model.selection of
                SelectedAddress a ->
                    Network.updateAddress a (s_selected False) model.network

                SelectedTx a ->
                    Network.updateTx a (Tx.updateUtxo (s_selected False)) model.network

                _ ->
                    model.network
    in
    network
        |> flip s_network model
        |> s_selection NoSelection


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
browserGotTxForAddress plugins _ addressId direction tx model =
    let
        network =
            Network.addTx tx model.network

        transform =
            case tx of
                Api.Data.TxTxUtxo t ->
                    Dict.get (Id.init t.currency t.txHash) network.txs
                        |> Maybe.andThen Tx.getUtxoTx
                        |> Maybe.map
                            (\t_ ->
                                Transform.move
                                    { x = t_.x * unit
                                    , y = A.getTo t_.y * unit
                                    , z = Transform.initZ
                                    }
                                    model.transform
                            )

                Api.Data.TxTxAccount _ ->
                    Nothing

        newmodel =
            { model
                | network = network
                , transform = Maybe.withDefault model.transform transform
            }

        address =
            Id.id addressId

        getBiggest io =
            Maybe.withDefault [] io
                |> List.filter (.address >> List.all ((/=) address))
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
                loadAddress plugins (Id.init (Id.network addressId) a) newmodel
            )
        |> Maybe.withDefault (n newmodel)


checkSelection : Model -> Model
checkSelection model =
    case model.selection of
        WillSelectTx id ->
            selectTx id model

        WillSelectAddress id ->
            selectAddress id model

        _ ->
            model


openAddressTransactionsTable : Model -> ( Model, List Effect )
openAddressTransactionsTable model =
    case model.details of
        Just (Details.Address id ad) ->
            let
                ( new, eff ) =
                    AddressDetails.showTransactionsTable id ad True
            in
            ( { model
                | details = Just (Details.Address id new)
              }
            , eff
            )

        _ ->
            n model


removeAddress : Id -> Model -> ( Model, List Effect )
removeAddress id model =
    ( { model
        | network = Network.deleteAddress id model.network
        , details =
            case model.details of
                Just (Details.Address addressId _) ->
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
    , []
    )
