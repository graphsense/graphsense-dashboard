module Update.Pathfinder exposing (update, updateByRoute)

import Api.Data
import Config.Update as Update
import Dict
import Dict.Nonempty as NDict
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
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network
import Model.Pathfinder.Table as PT
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Model.Search as Search
import Msg.Pathfinder as Msg exposing (AddressDetailsMsg(..), Msg(..))
import Msg.Search as Search
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Result.Extra
import Route.Pathfinder as Route
import Tuple exposing (first, pair, second)
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
                |> selectAddress id
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
                    }
                |> ApiEffect
                |> List.singleton
            )

        UserClickedAddress id ->
            ( selectAddress id model
            , Route.addressRoute
                { network = Id.network id
                , address = Id.id id
                }
                |> NavPushRouteEffect
                |> List.singleton
            )


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
            addressFromRoute plugins (Id.init network a) model

        _ ->
            n model


addressFromRoute : Plugins -> Id -> Model -> ( Model, List Effect )
addressFromRoute plugins id model =
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


addAddress : Plugins -> Id -> Api.Data.Address -> Model -> ( Model, List Effect )
addAddress plugins id data model =
    { model
        | network = Network.updateAddress id (s_data (Success data)) model.network
    }
        |> n


selectAddress : Id -> Model -> Model
selectAddress id model =
    let
        m1 =
            (s_selection (SelectedAddress id)
                >> (setViewState <| s_detailsViewState (AddressDetails id (getAddressDetailsViewStateDefaultForAddress id model)))
            )
                model
    in
    { m1 | network = Model.Pathfinder.Network.selectAddress (Model.Pathfinder.Network.unSelectAll m1.network) id }


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
browserGotTxForAddress _ _ id direction data model =
    getAddress model.network.addresses id
        |> Result.map (\{ x, y } -> Coords x y)
        |> Result.andThen (Tx.fromData data direction)
        |> Result.map
            (\tx ->
                let
                    nw =
                        Network.addTx tx model.network
                            |> Network.addAddress firstAddress

                    getBiggest io =
                        NDict.toList io
                            |> List.sortBy (second >> .value)
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault (NDict.head io)
                            |> first

                    firstAddress =
                        case tx.type_ of
                            Tx.Utxo t ->
                                case direction of
                                    Incoming ->
                                        getBiggest t.inputs

                                    Outgoing ->
                                        getBiggest t.outputs

                            Tx.Account t ->
                                case direction of
                                    Incoming ->
                                        t.from

                                    Outgoing ->
                                        t.to
                in
                ( { model | network = nw }
                , []
                )
            )
        |> Result.Extra.extract
            (ErrorEffect >> List.singleton >> pair model)


appendPagedTableData : PT.PagedTable p -> GT.Filter p -> Maybe String -> List p -> PT.PagedTable p
appendPagedTableData pt f nextPage data =
    { pt
        | t =
            appendData f data pt.t
                |> s_nextpage nextPage
                |> s_loading False
    }
