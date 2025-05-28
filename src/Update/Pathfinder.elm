module Update.Pathfinder exposing (deserialize, fromDeserialized, removeAddress, unselect, update, updateByPluginOutMsg, updateByRoute)

import Animation as A
import Api.Data
import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Config.Pathfinder exposing (nodeXOffset)
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
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Init.Pathfinder.TxDetails as TxDetails
import Json.Decode
import List.Extra
import Log
import Maybe.Extra
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (relativeToGraphZero)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Notification as Notification
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address as Addr exposing (Address, Txs(..), expandAllowed, getTxs, txsSetter)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.CheckingNeighbors as CheckingNeighbors
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Deserialize exposing (Deserialized)
import Model.Pathfinder.Error exposing (Error(..), InfoError(..))
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (FindPosition(..), Network)
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
        )
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Search as Search
import Number.Bounded exposing (value)
import PagedTable
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
import Tuple exposing (first, mapFirst, mapSecond, pair, second)
import Tuple2 exposing (pairTo)
import Tuple3
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Transform as Transform
import Update.Pathfinder.AddressDetails as AddressDetails
import Update.Pathfinder.Network as Network exposing (ingestAddresses, ingestTxs)
import Update.Pathfinder.Node as Node
import Update.Pathfinder.TxDetails as TxDetails
import Update.Pathfinder.WorkflowNextTxByTime as WorkflowNextTxByTime
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Update.Search as Search
import Util exposing (and, n)
import Util.Annotations as Annotations
import Util.Data as Data
import Util.Pathfinder.History as History
import Util.Pathfinder.TagSummary as TagSummary
import Util.Tag as Tag
import View.Locale as Locale
import Workflow


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


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory plugins msg
        |> markDirty plugins msg
        |> updateByMsg plugins uc msg


resultLineToRoute : Search.ResultLine -> Route
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
        UserOpensDialogWindow windowType ->
            case windowType of
                TagsList id ->
                    ( model
                    , UserGotDataForTagsListDialog id
                        |> Api.GetAddressTagsEffect { currency = Id.network id, address = Id.id id, pagesize = 5000, nextpage = Nothing, includeBestClusterTag = True }
                        |> ApiEffect
                        |> List.singleton
                    )

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
            n model |> unselect |> Tuple.mapFirst (s_details Nothing)

        UserReleasedDeleteKey ->
            deleteSelection model

        UserPressedNormalKey key ->
            case ( model.modPressed, key ) of
                ( True, "z" ) ->
                    update plugins uc UserClickedUndo model

                ( True, "y" ) ->
                    update plugins uc UserClickedRedo model

                _ ->
                    n model

        UserReleasedNormalKey _ ->
            n model

        BrowserGotAddressData id position data ->
            if (not <| Network.isEmpty model.network) && Network.findAddressCoords id model.network == Nothing then
                let
                    onlyIds =
                        model.network.addresses
                            |> Dict.values
                            |> List.map (.id >> Id.id)
                            |> List.filter ((/=) (Id.id id))
                in
                if List.isEmpty onlyIds then
                    browserGotAddressData uc plugins id position data model

                else
                    let
                        getRelations dir =
                            BrowserGotRelationsToVisibleNeighbors id dir
                                |> Api.GetAddressNeighborsEffect
                                    { currency = Id.network id
                                    , address = Id.id id
                                    , isOutgoing = dir == Outgoing
                                    , onlyIds =
                                        onlyIds
                                            |> Just
                                    , includeLabels = False
                                    , includeActors = False
                                    , pagesize = Dict.size model.network.addresses
                                    , nextpage = Nothing
                                    }
                                |> ApiEffect
                    in
                    ( CheckingNeighbors.initAddress data model.checkingNeighbors
                        |> flip s_checkingNeighbors model
                    , [ getRelations Outgoing
                      , getRelations Incoming
                      ]
                    )

            else
                browserGotAddressData uc plugins id position data model

        BrowserGotRelationsToVisibleNeighbors id dir relations ->
            let
                neighborIds =
                    relations.neighbors
                        |> List.map (\{ address } -> Id.init address.currency address.address)

                newModel =
                    CheckingNeighbors.insert dir id neighborIds model.checkingNeighbors
                        |> flip s_checkingNeighbors model
            in
            if CheckingNeighbors.isEmpty id newModel.checkingNeighbors then
                CheckingNeighbors.getData id model.checkingNeighbors
                    |> Maybe.map
                        (\data ->
                            browserGotAddressData uc plugins id Auto data newModel
                        )
                    |> Maybe.withDefault (n newModel)

            else
                neighborIds
                    |> List.concatMap
                        (\addressId ->
                            getNextTxEffects model.network
                                addressId
                                (Direction.flip dir)
                                (Just id)
                        )
                    |> pair newModel

        BrowserGotClusterData addressId data ->
            let
                clusterId =
                    Id.initClusterId data.currency data.entity
            in
            n
                { model
                    | clusters = Dict.insert clusterId (Success data) model.clusters
                }
                |> and
                    (AddressDetails.browserGotClusterData addressId data
                        |> updateAddressDetails addressId
                    )

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

        AddressDetailsMsg addressId subm ->
            case subm of
                {-
                           -- we can omit querying tags here because the
                           -- entity addresses are only retrieved after
                           -- checking all tagged addresses of the entity
                   AddressDetails.BrowserGotEntityAddressesForRelatedAddressesTable addresses ->
                       let
                           network =
                               Id.network addressId
                       in
                       addresses.addresses
                           |> List.map (.address >> Id.init network)
                           |> fetchTagSummaryForIds False model.tagSummaries
                           |> List.singleton
                           |> pair model
                           |> and
                               (AddressDetails.update uc subm
                                   |> updateAddressDetails addressId
                               )
                -}
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

                AddressDetails.UserClickedAllTxCheckboxInTable ->
                    case model.details of
                        Just (AddressDetails _ (Success data)) ->
                            let
                                txIdsTable =
                                    data.txs.table
                                        |> PagedTable.getPage
                                        |> List.map Tx.getTxIdForAddressTx

                                allChecked =
                                    txIdsTable
                                        |> List.all (flip Dict.member model.network.txs)

                                deleteAcc txId ( m, eff ) =
                                    ( Network.deleteTx txId m.network
                                        |> flip s_network m
                                    , eff
                                    )

                                addAcc txId ( m, eff ) =
                                    loadTx True plugins txId m |> Tuple.mapSecond ((++) eff)
                            in
                            if allChecked then
                                let
                                    toRemove =
                                        txIdsTable
                                            |> List.filter (flip Dict.member model.network.txs)
                                in
                                toRemove
                                    |> List.foldl deleteAcc
                                        ( model
                                        , Notification.infoDefault (Locale.interpolated uc.locale "Removed {0} transactions" [ toRemove |> List.length |> String.fromInt ])
                                            |> Notification.map (s_isEphemeral True)
                                            |> Notification.map (s_showClose False)
                                            |> ShowNotificationEffect
                                            |> List.singleton
                                        )

                            else
                                let
                                    toAdd =
                                        txIdsTable
                                            |> List.filter (flip Dict.member model.network.txs >> not)
                                in
                                toAdd
                                    |> List.foldl addAcc
                                        ( model
                                        , Notification.infoDefault (Locale.interpolated uc.locale "Added {0} transactions" [ toAdd |> List.length |> String.fromInt ])
                                            |> Notification.map (s_isEphemeral True)
                                            |> Notification.map (s_showClose False)
                                            |> ShowNotificationEffect
                                            |> List.singleton
                                        )

                        _ ->
                            n model

                AddressDetails.UserClickedTxCheckboxInTable tx ->
                    let
                        addOrRemoveTx txId =
                            Dict.get txId model.network.txs
                                |> Maybe.map
                                    (\t ->
                                        let
                                            delNw =
                                                Network.deleteTx txId model.network
                                        in
                                        Tx.listAddressesForTx delNw.addresses t
                                            |> List.filter
                                                (second >> .id >> (/=) addressId)
                                            |> List.map second
                                            |> Network.deleteDanglingAddresses delNw
                                            |> flip s_network model
                                            |> n
                                    )
                                |> Maybe.Extra.withDefaultLazy
                                    (\_ -> loadTx True plugins txId model)
                    in
                    addOrRemoveTx (Tx.getTxIdForAddressTx tx)

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
                    |> unselect

            else
                n m1 |> unselect

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
                                    adr |> Addr.getCoords |> isinRect

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
                                    z
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

        UserMovesMouseOverUtxoTx id ->
            if model.hovered == HoveredTx id then
                n model

            else
                let
                    domId =
                        Id.toString id

                    maybeTT =
                        model.network.txs
                            |> Dict.get id
                            |> Maybe.map
                                (\tx ->
                                    case tx.type_ of
                                        Tx.Utxo t ->
                                            Tooltip.UtxoTx t

                                        Tx.Account t ->
                                            Tooltip.AccountTx t
                                )

                    hovered =
                        ( { model
                            | network = Network.updateTx id (s_hovered True) model.network
                            , hovered = HoveredTx id
                          }
                        , case maybeTT of
                            Just tt ->
                                OpenTooltipEffect { context = domId, domId = domId } tt |> List.singleton

                            _ ->
                                []
                        )
                in
                case model.details of
                    Just (TxDetails txid _) ->
                        if id /= txid then
                            hovered

                        else
                            n model

                    _ ->
                        hovered

        UserMovesMouseOverAddress id ->
            if model.hovered == HoveredAddress id then
                n model

            else
                let
                    domId =
                        Id.toString id

                    maybeTT =
                        model.network.addresses
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

                    showHover =
                        ( { model
                            | hovered = HoveredAddress id
                          }
                        , case maybeTT of
                            Just tt ->
                                OpenTooltipEffect { context = domId, domId = domId } tt |> List.singleton

                            _ ->
                                []
                        )
                in
                case model.details of
                    Just (AddressDetails aid _) ->
                        if id /= aid then
                            showHover

                        else
                            n model

                    _ ->
                        showHover

        UserMovesMouseOutAddress id ->
            ( unhover model, CloseTooltipEffect (Just { context = Id.toString id, domId = Id.toString id }) False |> List.singleton )

        ShowTextTooltip config ->
            ( model, OpenTooltipEffect { context = config.domId, domId = config.domId } (Tooltip.Text config.text) |> List.singleton )

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
                    , OpenTooltipEffect ctx tt |> List.singleton
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
                    , OpenTooltipEffect ctx tt
                        |> List.singleton
                    )

                _ ->
                    n model

        UserMovesMouseOutActorLabel ctx ->
            ( model, CloseTooltipEffect (Just ctx) True |> List.singleton )

        UserMovesMouseOutTagLabel ctx ->
            ( model, CloseTooltipEffect (Just ctx) True |> List.singleton )

        UserMovesMouseOutUtxoTx id ->
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
                            expandAddress uc address direction model

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
                let
                    modelS =
                        multiSelect model [ MSelectedAddress id ] True
                in
                n { modelS | details = Nothing }

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
                            loadAddress plugins aId m |> Tuple.mapSecond ((++) eff)
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

        BrowserGotTx pos loadAddresses tx ->
            let
                ( newTx, newNetwork ) =
                    Network.addTxWithPosition pos tx model.network
            in
            (model |> s_network newNetwork)
                |> checkSelection uc
                |> and
                    (if loadAddresses then
                        autoLoadAddresses plugins newTx

                     else
                        n
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

        UserOpensAddressAnnotationDialog id ->
            let
                ( mn, effn ) =
                    selectAddress uc id model

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
            n { model | network = ingestTxs model.network deserializing.deserialized.txs txs }

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
                    Route.Network (Id.network id) (Route.Address (Id.id id))

                ContextMenu.TransactionContextMenu id ->
                    Route.Network (Id.network id) (Route.Tx (Id.id id))

                ContextMenu.TransactionIdChevronActions id ->
                    Route.Network (Id.network id) (Route.Tx (Id.id id))

                ContextMenu.AddressIdChevronActions id ->
                    Route.Network (Id.network id) (Route.Address (Id.id id))
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
browserGotAddressData uc plugins id position data model =
    let
        ( newAddress, net ) =
            Network.addAddressWithPosition plugins position id model.network
                |> mapSecond (Network.updateAddress id (s_data (Success data)))

        ( details, eff ) =
            case model.details of
                Just (AddressDetails i ad) ->
                    if i == id then
                        case ad of
                            Success a ->
                                { a | data = data }
                                    |> Success
                                    |> AddressDetails id
                                    |> Just
                                    |> n

                            _ ->
                                Dict.get id net.addresses
                                    |> Maybe.map
                                        (\address -> AddressDetails.init net clusters uc.locale address.id data)
                                    |> Maybe.map (mapFirst Success)
                                    |> Maybe.map (mapFirst (AddressDetails id))
                                    |> Maybe.map (mapFirst Just)
                                    |> Maybe.withDefault (n model.details)

                    else
                        model.details
                            |> n

                _ ->
                    model.details
                        |> n

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
            Transform.move
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
        |> s_details details
        |> s_colors ncolors
        |> s_clusters clusters
        |> pairTo (fetchTagSummaryForId True model.tagSummaries id :: fetchActorsForAddress data model.actors ++ eff ++ effCluster)
        |> and (checkSelection uc)


handleTooltipMsg : Tag.Msg -> Model -> ( Model, List Effect )
handleTooltipMsg msg model =
    case msg of
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
                                            |> AddressDetails.TooltipMsg
                                            |> AddressDetailsMsg id
                                    , closeTooltip =
                                        Tag.UserMovesMouseOutTagConcept ctx
                                            |> AddressDetails.TooltipMsg
                                            |> AddressDetailsMsg id
                                    , openDetails = Just (UserOpensDialogWindow (TagsList id))
                                    }
                        in
                        case Dict.get id model.tagSummaries of
                            Just (HasTagSummaries { withCluster }) ->
                                ( model
                                , OpenTooltipEffect ctx (tsToTooltip withCluster) |> List.singleton
                                )

                            Just (HasTagSummaryWithCluster ts) ->
                                ( model
                                , OpenTooltipEffect ctx (tsToTooltip ts) |> List.singleton
                                )

                            Just (HasTagSummaryOnlyWithCluster ts) ->
                                ( model
                                , OpenTooltipEffect ctx (tsToTooltip ts) |> List.singleton
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
        loadAddress plugins id model


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
    let
        bbox =
            Network.getBoundingBox model.network

        bboxXUnit =
            { x = bbox.x * unit - unit
            , y = bbox.y * unit - unit
            , width = bbox.width * unit + (2 * unit)
            , height = bbox.height * unit + (2 * unit)
            }
    in
    { model
        | transform =
            uc.size
                |> Maybe.map
                    (\{ width, height, x } ->
                        { width = width - (x * 4) -- not sure why I need this offset
                        , height = height
                        }
                    )
                |> Maybe.map
                    (bboxXUnit
                        |> Transform.updateByBoundingBox model.transform
                    )
                |> Maybe.withDefault model.transform
    }


expandAddress : Update.Config -> Address -> Direction -> Model -> ( Model, List Effect )
expandAddress uc address direction model =
    let
        id =
            address.id

        ( newmodel, eff ) =
            model
                |> selectAddress uc id

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
                |> getNextTxEffects newmodel.network id direction
                |> (++) eff
            )


deleteSelection : Model -> ( Model, List Effect )
deleteSelection model =
    (case model.selection of
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

        _ ->
            n model
    )
        |> unselect


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


getNextTxEffects : Network -> Id -> Direction -> Maybe Id -> List Effect
getNextTxEffects network addressId direction neighborId =
    let
        config =
            { addressId = addressId
            , direction = direction
            }
    in
    Network.getRecentTxForAddress network (Direction.flip direction) addressId
        |> Maybe.map
            (\tx ->
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
            )
        |> Maybe.Extra.withDefaultLazy
            (\_ ->
                neighborId
                    |> Maybe.map (WorkflowNextTxByTime.startBetween config)
                    |> Maybe.withDefault (WorkflowNextTxByTime.start config)
                    |> Workflow.mapEffect (WorkflowNextTxByTime config neighborId)
                    |> Workflow.next
                    |> List.map ApiEffect
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
        forcePushHistory (model |> s_isDirty True)
            |> updateByRoute_ plugins uc route


addPathsToGraph : Plugins -> Update.Config -> Model -> String -> List (List PathHopType) -> ( Model, List Effect )
addPathsToGraph plugins uc model net listOfPaths =
    let
        baseModelUnselected =
            ( model, [] ) |> unselect
    in
    List.foldl
        (\paths ( m, eff ) ->
            addPathToGraph plugins uc m net paths
                |> Tuple.mapSecond ((++) eff)
        )
        baseModelUnselected
        listOfPaths


addPathToGraph : Plugins -> Update.Config -> Model -> String -> List PathHopType -> ( Model, List Effect )
addPathToGraph plugins uc model net list =
    let
        getAddress adr =
            case adr of
                Route.AddressHop _ a ->
                    Just a

                _ ->
                    Nothing

        pathTypeToAddressId pt =
            case pt of
                AddressHop _ x ->
                    Just (Id.init net x)

                _ ->
                    Nothing

        pathTypeToSelection pt =
            case pt of
                AddressHop _ x ->
                    MSelectedAddress (Id.init net x)

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
                        , 0
                        )

                x_ =
                    x + xOffset

                y_ =
                    y + yOffset

                action =
                    case a of
                        Route.AddressHop _ adr ->
                            loadAddressWithPosition plugins (Fixed x_ y_) ( net, adr )

                        Route.TxHop h ->
                            loadTxWithPosition (Fixed x_ y_) False plugins ( net, h )

                annotations =
                    case a of
                        Route.AddressHop VictimAddress adr ->
                            Annotations.set
                                ( net, adr )
                                (Locale.string uc.locale "victim")
                                (Just annotationGreen)
                                m.annotations

                        Route.AddressHop PerpetratorAddress adr ->
                            Annotations.set
                                ( net, adr )
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
            unselect (n model)

        Route.Network network (Route.Address a) ->
            let
                id =
                    Id.init network a
            in
            { model | network = Network.clearSelection model.network }
                |> loadAddress plugins id
                |> and (selectAddress uc id)

        Route.Network network (Route.Tx a) ->
            let
                id =
                    Id.init network a

                m1 =
                    { model | network = Network.clearSelection model.network }
            in
            loadTx True plugins id m1
                |> and (selectTx id)

        Route.Path net list ->
            addPathToGraph plugins uc model net list

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
                        addPathsToGraph plugins uc mo net paths
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
                        ( mo, [ OpenTooltipEffect s (Tooltip.Plugin s (Tooltip.mapMsgTooltipMsg msgs PluginMsg)) ] )

                    PluginInterface.CloseTooltip s withDelay ->
                        ( mo, [ CloseTooltipEffect (Just s) withDelay ] )
            )
            ( model, [] )


loadAddress : Plugins -> Id -> Model -> ( Model, List Effect )
loadAddress plugins =
    loadAddressWithPosition plugins Auto


loadAddressWithPosition : Plugins -> FindPosition -> Id -> Model -> ( Model, List Effect )
loadAddressWithPosition _ position id model =
    if Dict.member id model.network.addresses then
        n model

    else
        ( -- don't add the address here because it is not loaded yet
          --{ model | network = Network.addAddressWithPosition plugins position id model.network }
          model
        , [ BrowserGotAddressData id position
                |> Api.GetAddressEffect
                    { currency = Id.network id
                    , address = Id.id id
                    , includeActors = True
                    }
                |> ApiEffect
          ]
        )


loadTxWithPosition : FindPosition -> Bool -> Plugins -> Id -> Model -> ( Model, List Effect )
loadTxWithPosition pos loadAddresses _ id model =
    if Dict.member id model.network.txs then
        n model

    else
        ( model
        , BrowserGotTx pos loadAddresses
            |> Api.GetTxEffect
                { currency = Id.network id
                , txHash = Id.id id
                , includeIo = True
                , tokenTxId = Nothing
                }
            |> ApiEffect
            |> List.singleton
        )


loadTx : Bool -> Plugins -> Id -> Model -> ( Model, List Effect )
loadTx =
    loadTxWithPosition Auto


selectTx : Id -> Model -> ( Model, List Effect )
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

                ( m1, eff ) =
                    unselect (n model)
                        |> Tuple.mapFirst
                            (s_details (TxDetails.init tx |> TxDetails id |> Just))
            in
            selectedTx
                |> Maybe.map (\a -> Network.updateTx a (s_selected False) m1.network)
                |> Maybe.withDefault m1.network
                |> Network.updateTx id (s_selected True)
                |> flip s_network m1
                |> s_selection (SelectedTx id)
                |> bulkfetchTagsForTx tx
                |> Tuple.mapSecond ((++) eff)

        Nothing ->
            s_selection (WillSelectTx id) model
                |> n


selectAddress : Update.Config -> Id -> Model -> ( Model, List Effect )
selectAddress uc id model =
    case Dict.get id model.network.addresses of
        Just address ->
            let
                newDetails =
                    address.data
                        |> RemoteData.map (AddressDetails.init model.network model.clusters uc.locale address.id)

                details =
                    case model.details of
                        Just (AddressDetails i data) ->
                            if id == i then
                                -- keep it unchanged
                                data
                                    |> RemoteData.map n

                            else
                                newDetails

                        _ ->
                            newDetails

                eff =
                    details
                        |> RemoteData.toMaybe
                        |> Maybe.map second
                        |> Maybe.withDefault []

                ( m1, eff2 ) =
                    unselect ( model, eff )
                        |> Tuple.mapFirst
                            (s_details
                                (RemoteData.map first details
                                    |> AddressDetails id
                                    |> Just
                                )
                            )
            in
            Network.updateAddress id (s_selected True) m1.network
                |> flip s_network m1
                |> s_selection (SelectedAddress id)
                |> pairTo eff2

        Nothing ->
            s_selection (WillSelectAddress id) model
                |> n


unselect : ( Model, List Effect ) -> ( Model, List Effect )
unselect ( model, eff ) =
    let
        unselectAddress a nw =
            Network.updateAddress a (s_selected False) nw

        unselectTx a nw =
            Network.updateTx a (s_selected False) nw

        network =
            case model.selection of
                SelectedAddress a ->
                    unselectAddress a model.network

                SelectedTx a ->
                    unselectTx a model.network

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

                NoSelection ->
                    model.network
    in
    ( network
        |> flip s_network model
        |> s_details Nothing
        |> s_selection NoSelection
    , eff ++ [ CloseTooltipEffect Nothing False ]
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

                _ ->
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
    { network = (unselect (n model) |> Tuple.first |> unhover).network
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
addTx plugins _ anchorAddressId direction addressId tx model =
    let
        ( newTx, network ) =
            Network.addTxWithPosition (Network.NextTo ( direction, anchorAddressId )) tx model.network

        transform =
            Transform.move
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
                loadAddressWithPosition plugins position a newmodel
            )
        |> Maybe.withDefault (n newmodel)


checkSelection : Update.Config -> Model -> ( Model, List Effect )
checkSelection uc model =
    case model.selection of
        WillSelectTx id ->
            selectTx id model

        WillSelectAddress id ->
            selectAddress uc id model

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
        groupByNetwork =
            List.map .id
                >> List.Extra.gatherEqualsBy first
                >> List.map (\( fst, more ) -> ( first fst, second fst :: List.map second more ))

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

        ( newAndEmptyPathfinder, _ ) =
            Pathfinder.init
                { snapToGrid = Just model.config.snapToGrid
                , highlightClusterFriends = Just model.config.highlightClusterFriends
                }
    in
    ( { newAndEmptyPathfinder
        | network = ingestAddresses plugins Network.init deserialized.addresses
        , annotations = List.foldl (\i m -> Annotations.set i.id i.label i.color m) model.annotations deserialized.annotations
        , history = History.init
        , name = deserialized.name
      }
    , txsRequests
        ++ addressesRequests
    )


autoLoadAddresses : Plugins -> Tx -> Model -> ( Model, List Effect )
autoLoadAddresses plugins tx model =
    let
        addresses =
            Tx.listAddressesForTx model.network.addresses tx
                |> List.map first

        aggAddressAdd addressId =
            and (loadAddress plugins addressId)

        src =
            if List.member Incoming addresses then
                Nothing

            else
                getAddressForDirection tx Incoming Set.empty

        dst =
            if List.member Outgoing addresses then
                Nothing

            else
                (tx |> Tx.getInputAddressIds)
                    |> Set.fromList
                    |> getAddressForDirection tx Outgoing
    in
    [ src, dst ]
        |> List.filterMap identity
        |> List.foldl aggAddressAdd (n model)


updateAddressDetails : Id -> (AddressDetails.Model -> ( AddressDetails.Model, List Effect )) -> Model -> ( Model, List Effect )
updateAddressDetails id upd model =
    case model.details of
        Just (AddressDetails id_ (Success ad)) ->
            if id == id_ then
                let
                    ( addressViewDetails, eff ) =
                        upd ad
                in
                ( { model
                    | details =
                        Success addressViewDetails
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

                        ( _, _ ) ->
                            curr
                )
            )
            dict

    else
        Dict.insert id newTagSummary dict
