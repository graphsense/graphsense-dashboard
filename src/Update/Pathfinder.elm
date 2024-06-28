module Update.Pathfinder exposing (update, updateByRoute)

import Animation as A
import Api.Data
import Basics.Extra exposing (flip)
import Config.Update as Update
import Dict
import Dict.Nonempty as NDict
import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Init.Graph.Transform as Transform
import Init.Pathfinder.AddressDetails as AddressDetails
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Init.Pathfinder.TxDetails as TxDetails
import List.Extra
import Log
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (relativeToGraphZero)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale exposing (State(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id, network)
import Model.Pathfinder.Network as Network
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Model.Pathfinder.Tx as Tx
import Model.Search as Search
import Msg.Pathfinder as Msg
    exposing
        ( DisplaySettingsMsg(..)
        , Msg(..)
        , TxDetailsMsg(..)
        , WorkflowNextTxByTimeMsg(..)
        , WorkflowNextUtxoTxMsg(..)
        )
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Search as Search
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Route.Pathfinder as Route
import Set
import Svg.Attributes exposing (x)
import Time
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


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory msg
        |> markDirty msg
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
                            (s_exchange (Just data.label))
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
            n { model | ctrlPressed = True }

        UserReleasedCtrlKey ->
            n { model | ctrlPressed = False }

        UserPressedDeleteKey ->
            case model.selection of
                SelectedAddress id ->
                    removeAddress id model

                SelectedTx id ->
                    removeTx id model

                MultiSelect items ->
                    List.foldl
                        (\i ( m, eff ) ->
                            case i of
                                MSelectedAddress id ->
                                    removeAddress id m

                                MSelectedTx id ->
                                    removeTx id m
                        )
                        ( model, [] )
                        items

                _ ->
                    n model

        UserPressedNormalKey key ->
            case key of
                "z" ->
                    update plugins uc UserClickedUndo model

                "y" ->
                    update plugins uc UserClickedRedo model

                _ ->
                    n model

        UserReleasedNormalKey _ ->
            n model

        BrowserGotAddressData id data ->
            let
                net =
                    Network.updateAddress id (s_data (Success data)) model.network

                net2 =
                    Network.updateAddress id (s_data (Success data)) net
            in
            model
                |> s_network net2
                |> selectAddress uc id
                |> pairTo (fetchTagsForAddress data model.tags :: fetchActorsForAddress data model.actors)

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
            { model | details = Nothing, selection = NoSelection }
                |> n

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

        AddressDetailsMsg subm ->
            case model.details of
                Just (AddressDetails id (Success ad)) ->
                    let
                        ( addressViewDetails, eff ) =
                            AddressDetails.update subm id ad
                    in
                    ( { model | details = Just (AddressDetails id (Success addressViewDetails)) }, eff )

                _ ->
                    n model

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

        --n Init.Pathfinder.init
        -- TODO: Implement
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
                        |> selectAddress uc id
                        |> openAddressTransactionsTable
            in
            ( newmodel
            , getNextTxEffects newmodel id direction
                ++ eff
            )

        UserClickedAddress id ->
            if model.ctrlPressed then
                let
                    nn =
                        Network.updateAddress id (s_selected True) model.network

                    nselect =
                        case model.selection of
                            MultiSelect x ->
                                MultiSelect (MSelectedAddress id :: x)

                            SelectedAddress oid ->
                                MultiSelect [ MSelectedAddress id, MSelectedAddress oid ]

                            SelectedTx oid ->
                                MultiSelect [ MSelectedAddress id, MSelectedTx oid ]

                            _ ->
                                MultiSelect [ MSelectedAddress id ]
                in
                n { model | selection = nselect, details = Nothing, network = nn }

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
            if Dict.member id model.network.addresses then
                removeAddress id model

            else
                loadAddress plugins id model False

        UserClickedTx id ->
            if model.ctrlPressed then
                let
                    nn =
                        Network.updateTx id (Tx.updateUtxo (s_selected True)) model.network

                    nselect =
                        case model.selection of
                            MultiSelect x ->
                                MultiSelect (MSelectedTx id :: x)

                            SelectedAddress oid ->
                                MultiSelect [ MSelectedTx id, MSelectedAddress oid ]

                            SelectedTx oid ->
                                MultiSelect [ MSelectedTx id, MSelectedTx oid ]

                            _ ->
                                MultiSelect [ MSelectedTx id ]
                in
                n { model | selection = nselect, details = Nothing, network = nn }

            else
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
                |> checkSelection uc
                |> n

        ChangedDisplaySettingsMsg submsg ->
            case submsg of
                ChangePointerTool tool ->
                    n { model | pointerTool = tool }

                UserClickedToggleShowTxTimestamp ->
                    let
                        nds =
                            model.displaySettings |> s_showTxTimestamps (not model.displaySettings.showTxTimestamps)
                    in
                    n { model | displaySettings = nds }

                UserClickedToggleDisplaySettings ->
                    let
                        nds =
                            model.displaySettings |> s_isDisplaySettingsOpen (not model.displaySettings.isDisplaySettingsOpen)
                    in
                    n { model | displaySettings = nds }

        Tick time ->
            n { model | currentTime = time }

        WorkflowNextUtxoTx context wm ->
            WorkflowNextUtxoTx.update context wm model

        WorkflowNextTxByTime context wm ->
            WorkflowNextTxByTime.update context wm model

        BrowserGotAddressTags id data ->
            if not (List.isEmpty data.addressTags) then
                n
                    { model
                        | tags = Dict.insert id data model.tags
                        , network = Network.updateAddress id (s_hasTags True) model.network
                    }

            else
                n model


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


updateByRoute : Plugins -> Update.Config -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute plugins uc route model =
    forcePushHistory (model |> s_isDirty True)
        |> updateByRoute_ plugins uc route


updateByRoute_ : Plugins -> Update.Config -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute_ plugins uc route model =
    case route |> Log.log "route" of
        Route.Root ->
            n model

        Route.Network network (Route.Address a) ->
            let
                id =
                    Id.init network a

                m1 =
                    { model | network = Network.clearSelection model.network }
            in
            loadAddress plugins id m1 True
                |> mapFirst (selectAddress uc id)

        Route.Network network (Route.Tx a) ->
            let
                id =
                    Id.init network a

                m1 =
                    { model | network = Network.clearSelection model.network }
            in
            loadTx plugins id m1
                |> mapFirst (selectTx id)

        _ ->
            n model


loadAddress : Plugins -> Id -> Model -> Bool -> ( Model, List Effect )
loadAddress _ id model starting =
    if Dict.member id model.network.addresses then
        n model

    else
        let
            is_new =
                not (Dict.member id model.network.addresses)

            nw =
                Network.addAddress id model.network
                    |> Network.updateAddress id (s_data Loading)

            nw2 =
                if is_new then
                    Network.updateAddress id (s_isStartingPoint (starting && is_new)) nw

                else
                    nw
        in
        ( { model | network = nw2 }
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
    case Dict.get id model.network.txs of
        Just tx ->
            let
                selectedTx =
                    case model.selection of
                        SelectedTx a ->
                            Just a

                        _ ->
                            Nothing

                m1 =
                    unselect model
                        |> s_details (TxDetails.init tx |> TxDetails id |> Just)
            in
            selectedTx
                |> Maybe.map (\a -> Network.updateTx a (Tx.updateUtxo (s_selected False)) m1.network)
                |> Maybe.withDefault m1.network
                |> Network.updateTx id (Tx.updateUtxo (s_selected True))
                |> flip s_network m1
                |> s_selection (SelectedTx id)

        Nothing ->
            s_selection (WillSelectTx id) model


selectAddress : Update.Config -> Id -> Model -> Model
selectAddress uc id model =
    case Dict.get id model.network.addresses of
        Just address ->
            let
                details =
                    address.data
                        |> RemoteData.map (AddressDetails.init uc.locale)

                m1 =
                    unselect model
                        |> s_details (AddressDetails id details |> Just)
            in
            Network.updateAddress id (s_selected True) m1.network
                |> flip s_network m1
                |> s_selection (SelectedAddress id)

        Nothing ->
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
                    , selection = NoSelection
                }
            )
        |> Maybe.withDefault model
        |> n


markDirty : Msg -> Model -> Model
markDirty msg model =
    if History.shallPushHistory msg model then
        model |> s_isDirty True

    else
        model


fetchActor : String -> Effect
fetchActor id =
    BrowserGotActor id |> Api.GetActorEffect { actorId = id } |> ApiEffect


fetchTags : Id -> Effect
fetchTags id =
    BrowserGotAddressTags id |> Api.GetAddressTagsEffect { currency = Id.network id, address = Id.id id, pagesize = 1000, nextpage = Nothing } |> ApiEffect


fetchTagsForAddress : Api.Data.Address -> Dict.Dict Id Api.Data.AddressTags -> Effect
fetchTagsForAddress d existing =
    let
        id =
            ( d.currency, d.address )
    in
    if Dict.member id existing then
        CmdEffect Cmd.none

    else
        fetchTags id


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
                loadAddress plugins (Id.init (Id.network addressId) a) newmodel False
            )
        |> Maybe.withDefault (n newmodel)


checkSelection : Update.Config -> Model -> Model
checkSelection uc model =
    case model.selection of
        WillSelectTx id ->
            selectTx id model

        WillSelectAddress id ->
            selectAddress uc id model

        _ ->
            model


openAddressTransactionsTable : Model -> ( Model, List Effect )
openAddressTransactionsTable model =
    case model.details of
        Just (AddressDetails id (Success ad)) ->
            let
                ( new, eff ) =
                    AddressDetails.showTransactionsTable id ad True
            in
            ( { model
                | details = Just (AddressDetails id (Success new))
              }
            , eff
            )

        _ ->
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
        , details =
            case model.details of
                Just (TxDetails txId _) ->
                    if txId == id then
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


isIsolatedTx : Model -> Tx.Tx -> Bool
isIsolatedTx model tx =
    case tx.type_ of
        Tx.Utxo x ->
            let
                keys =
                    (x.outputs |> NDict.toDict |> Dict.keys) ++ (x.inputs |> NDict.toDict |> Dict.keys)
            in
            not (List.any (\y -> Dict.member y model.network.addresses) keys)

        Tx.Account x ->
            Dict.member x.from model.network.addresses || Dict.member x.to model.network.addresses


removeIsolatedTransactions : Model -> ( Model, List Effect )
removeIsolatedTransactions model =
    let
        idsToRemove =
            Dict.keys (Dict.filter (\k v -> isIsolatedTx model v) model.network.txs)
    in
    List.foldl (\i ( m, _ ) -> removeTx i m) ( model, [] ) idsToRemove
