module Update.Pathfinder exposing (deserialize, fromDeserialized, update, updateByRoute)

import Animation as A
import Api.Data
import Basics.Extra exposing (flip)
import Config.Update as Update
import Css.Pathfinder exposing (searchBoxMinWidth)
import Decode.Pathfinder1
import Dict
import Effect exposing (and, n)
import Effect.Api as Api exposing (Effect(..))
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Hovercard
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Pathfinder.AddressDetails as AddressDetails
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Init.Pathfinder.Tooltip as Tooltip
import Init.Pathfinder.TxDetails as TxDetails
import Json.Decode
import List.Extra
import Log
import Model.Direction as Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (relativeToGraphZero)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale exposing (State(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address as Addr exposing (Txs(..), getTxs, txsSetter)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Deserialize exposing (Deserialized)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id, network)
import Model.Pathfinder.Network as Network
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Model.Pathfinder.Tooltip as Tooltip
import Model.Pathfinder.Tx as Tx exposing (Tx)
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
import Number.Bounded exposing (value)
import Plugin.Update as Plugin exposing (Plugins)
import Ports
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Route.Pathfinder as Route exposing (Route(..))
import Set exposing (..)
import Svg.Attributes exposing (x)
import Tuple exposing (first, mapFirst, mapSecond, second)
import Tuple2 exposing (pairTo)
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Table exposing (UpdateSearchTerm(..))
import Update.Graph.Transform as Transform
import Update.Pathfinder.AddressDetails as AddressDetails
import Update.Pathfinder.Network as Network exposing (FindPosition(..), ingestAddresses, ingestTxs)
import Update.Pathfinder.Node as Node
import Update.Pathfinder.Tx as Tx
import Update.Pathfinder.TxDetails as TxDetails
import Update.Pathfinder.WorkflowNextTxByTime as WorkflowNextTxByTime
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Update.Search as Search
import Util.Data as Data exposing (timestampToPosix)
import Util.Pathfinder.History as History
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)


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

        UserClickedExportGraphAsPNG _ ->
            -- handled in src/Update.elm
            n model

        UserClickedSaveGraph _ ->
            -- handled in src/Update.elm
            n model

        NoOp ->
            n model

        BrowserGotActor id data ->
            n
                { model
                    | actors = Dict.insert id data model.actors
                }

        UserPressedModKey ->
            n { model | modPressed = True }

        UserReleasedModKey ->
            n { model | modPressed = False }

        UserReleasedEscape ->
            n (model |> unselect |> s_details Nothing)

        UserReleasedDeleteKey ->
            deleteSelection model

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
                                                (\address -> AddressDetails.init net uc.locale address.id data)
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
                    Network.isClusterFriendAlreadyOnGraph clusterId model.network

                ncolors =
                    if isSecondAddressFromSameCluster then
                        Colors.assignNextColor Colors.Clusters clusterId model.colors

                    else
                        model.colors

                effwithCluster =
                    eff
                        ++ (if Dict.member clusterId model.clusters || Data.isAccountLike data.currency then
                                []

                            else
                                [ BrowserGotClusterData clusterId |> Api.GetEntityEffectWithDetails { currency = Id.network id, entity = data.entity, includeActors = False, includeBestTag = False } |> ApiEffect ]
                           )
            in
            model
                |> s_network net
                |> s_details details
                |> s_colors ncolors
                |> pairTo (fetchTagSummaryForId model.tagSummaries id :: fetchActorsForAddress data model.actors ++ effwithCluster)

        BrowserGotClusterData id data ->
            n { model | clusters = Dict.insert id data model.clusters }

        BrowserGotTxForAddress addressId direction data ->
            let
                ( m, eff ) =
                    addTx plugins uc addressId direction data model
            in
            ( m, eff )

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
                            AddressDetails.update uc model subm id ad
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

                m1 =
                    model |> s_tooltip Nothing |> s_config (model.config |> s_displaySettingsHovercard Nothing)
            in
            if click then
                ( m1
                , Route.Root
                    |> NavPushRouteEffect
                    |> List.singleton
                )

            else
                n m1

        UserClickedFitGraph ->
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
            n
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

        UserReleasesMouseButton ->
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

                                ( modelS, _ ) =
                                    multiSelect model (selectedTxs ++ selectedAdr) False
                            in
                            n
                                { modelS
                                    | dragging = NoDragging
                                    , pointerTool = Drag
                                }

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
            { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            Dragging model.transform (relativeToGraphZero uc.size coords) (relativeToGraphZero uc.size coords)

                        _ ->
                            NoDragging
                , tooltip = Nothing
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
                , tooltip = Nothing
            }
                |> n

        UserMovesMouseOverUtxoTx id ->
            if model.hovered == HoveredTx id then
                n model

            else
                let
                    ( hc, cmd ) =
                        Id.toString id
                            |> Hovercard.init

                    hovered =
                        ( { model
                            | tooltip =
                                model.network.txs
                                    |> Dict.get id
                                    |> Maybe.andThen
                                        (\tx ->
                                            case tx.type_ of
                                                Tx.Utxo t ->
                                                    Tooltip.UtxoTx t
                                                        |> Tooltip.init hc
                                                        |> Just

                                                Tx.Account t ->
                                                    Tooltip.AccountTx t
                                                        |> Tooltip.init hc
                                                        |> Just
                                        )
                            , network = Network.updateTx id (s_hovered True) model.network
                            , hovered = HoveredTx id
                          }
                        , Cmd.map HovercardMsg cmd
                            |> CmdEffect
                            |> List.singleton
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
                    ( hc, cmd ) =
                        Id.toString id
                            |> Hovercard.init

                    showHover =
                        ( { model
                            | tooltip =
                                model.network.addresses
                                    |> Dict.get id
                                    |> Maybe.andThen
                                        (\addr ->
                                            Tooltip.Address addr |> Tooltip.init hc |> Just
                                        )

                            -- , network = Network.updateTx id (s_hovered True) model.network
                            , hovered = HoveredAddress id
                          }
                        , Cmd.map HovercardMsg cmd
                            |> CmdEffect
                            |> List.singleton
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
            { model
                | tooltip = Nothing
                , hovered =
                    if model.hovered == HoveredAddress id then
                        NoHover

                    else
                        model.hovered
            }
                |> n

        UserMovesMouseOverTagLabel x ->
            case model.details of
                Just (AddressDetails id _) ->
                    let
                        ( hc, cmd ) =
                            x |> Hovercard.init
                    in
                    ( { model
                        | tooltip =
                            case Dict.get id model.tagSummaries of
                                Just (HasTagSummary ts) ->
                                    Just (Tooltip.TagLabel x ts |> Tooltip.init hc)

                                _ ->
                                    Nothing
                      }
                    , Cmd.map HovercardMsg cmd
                        |> CmdEffect
                        |> List.singleton
                    )

                _ ->
                    n model

        UserMovesMouseOutTagLabel _ ->
            n { model | tooltip = Nothing }

        HovercardMsg hcMsg ->
            model.tooltip
                |> Maybe.map
                    (\tooltip ->
                        let
                            ( hc, cmd ) =
                                Hovercard.update hcMsg tooltip.hovercard
                        in
                        ( { model
                            | tooltip = Just { tooltip | hovercard = hc }
                          }
                        , Cmd.map HovercardMsg cmd
                            |> CmdEffect
                            |> List.singleton
                        )
                    )
                |> Maybe.withDefault (n model)

        UserMovesMouseOutUtxoTx id ->
            { model
                | tooltip = Nothing
                , network = Network.updateTx id (s_hovered False) model.network
                , hovered =
                    if model.hovered == HoveredTx id then
                        NoHover

                    else
                        model.hovered
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
                , tooltip = Nothing
            }
                |> n

        UserMovesMouseOnGraph coords ->
            case model.dragging of
                NoDragging ->
                    n (model |> s_tooltip Nothing)

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
            , Maybe.map
                (.hovercard
                    >> Hovercard.getElement
                    >> Cmd.map HovercardMsg
                    >> CmdEffect
                    >> List.singleton
                )
                model.tooltip
                |> Maybe.withDefault []
            )

        UserClickedAddressExpandHandle id direction ->
            Dict.get id model.network.addresses
                |> Maybe.map
                    (\address ->
                        let
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
                                , WorkflowNextUtxoTx.loadReferencedTx
                                    { addressId = id
                                    , direction = direction
                                    , hops = 0
                                    }
                                    tx
                                    |> List.singleton
                                )

                            TxsNotFetched ->
                                ( newmodel |> setLoading
                                , getNextTxEffects newmodel id direction
                                    ++ eff
                                )
                    )
                |> Maybe.withDefault (n model)

        UserClickedAddress id ->
            if model.modPressed then
                let
                    ( modelS, _ ) =
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
            if Dict.member id model.network.addresses then
                removeAddress id model

            else
                loadAddress plugins id model

        UserClickedTx id ->
            if model.modPressed then
                let
                    ( modelS, _ ) =
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

        UserClickedTxCheckboxInTable tx ->
            let
                addOrRemoveTx txId =
                    if Dict.member txId model.network.txs then
                        Network.deleteTx txId model.network
                            |> flip s_network model
                            |> n

                    else
                        loadTx plugins txId model
            in
            case tx of
                Api.Data.AddressTxTxAccount _ ->
                    addOrRemoveTx (Tx.getTxId2 tx)

                Api.Data.AddressTxAddressTxUtxo _ ->
                    addOrRemoveTx (Tx.getTxId2 tx)

        UserClickedRemoveAddressFromGraph id ->
            removeAddress id model

        BrowserGotTx tx ->
            let
                ( newTx, newNetwork ) =
                    Network.addTx tx model.network
            in
            (model |> s_network newNetwork)
                |> checkSelection uc
                |> and (autoLoadAddresses plugins newTx)

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
            let
                toEffect =
                    Cmd.map
                        (DisplaySettingsHovercardMsg >> ChangedDisplaySettingsMsg)
                        >> CmdEffect
                        >> List.singleton

                updateHovercard =
                    flip s_displaySettingsHovercard model.config
                        >> flip s_config model
            in
            case submsg of
                UserClickedToggleSnapToGrid ->
                    -- handled Upstream
                    n (model |> s_config (model.config |> s_snapToGrid (not model.config.snapToGrid)))

                UserClickedToggleShowTxTimestamp ->
                    -- handled Upstream
                    n model

                UserClickedToggleDatesInUserLocale ->
                    -- handled Uptream
                    n model

                UserClickedToggleShowTimeZoneOffset ->
                    -- handled Uptream
                    n model

                UserClickedToggleHighlightClusterFriends ->
                    -- handled Uptream
                    n model

                UserClickedToggleDisplaySettings ->
                    case model.config.displaySettingsHovercard of
                        Nothing ->
                            "toolbar-display-settings"
                                |> Hovercard.init
                                |> mapFirst (Just >> updateHovercard)
                                |> mapSecond toEffect

                        Just _ ->
                            s_displaySettingsHovercard Nothing model.config
                                |> flip s_config model
                                |> n

                DisplaySettingsHovercardMsg hcMsg ->
                    model.config.displaySettingsHovercard
                        |> Maybe.map
                            (\hc ->
                                Hovercard.update hcMsg hc
                                    |> mapFirst (Just >> updateHovercard)
                                    |> mapSecond toEffect
                            )
                        |> Maybe.withDefault (n model)

        UserClickedToggleClusterDetailsOpen ->
            n (model |> s_config (model.config |> s_isClusterDetailsOpen (not model.config.isClusterDetailsOpen)))

        UserClickedToggleDisplayAllTagsInDetails ->
            n (model |> s_config (model.config |> s_displayAllTagsInDetails (not model.config.displayAllTagsInDetails)))

        Tick time ->
            n { model | currentTime = time }

        WorkflowNextUtxoTx context wm ->
            WorkflowNextUtxoTx.update context wm model

        WorkflowNextTxByTime context wm ->
            WorkflowNextTxByTime.update context wm model

        BrowserGotTagSummary id data ->
            let
                d =
                    if data.tagCount > 0 then
                        HasTagSummary data

                    else
                        NoTags
            in
            ( { model
                | tagSummaries = Dict.insert id d model.tagSummaries
              }
                |> updateTagDataOnAddress id
            , data.bestActor |> Maybe.map (List.singleton >> flip fetchActors model.actors) |> Maybe.withDefault []
            )

        BrowserGotAddressesTags _ data ->
            let
                updateHasTags ( id, tag ) =
                    Dict.update id
                        (Maybe.map
                            (\curr ->
                                case ( curr, tag ) of
                                    ( HasTagSummary _, _ ) ->
                                        curr

                                    ( _, Just _ ) ->
                                        HasTags

                                    ( _, Nothing ) ->
                                        NoTags
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

        BrowserGotBulkAddresses addresses ->
            addresses
                |> List.foldl
                    (\address mod ->
                        and (updateByMsg plugins uc (BrowserGotAddressData (Id.init address.currency address.address) address)) mod
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


deleteSelection : Model -> ( Model, List Effect )
deleteSelection model =
    case model.selection of
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


updateTagDataOnAddress : Id -> Model -> Model
updateTagDataOnAddress addressId m =
    let
        tag =
            Dict.get addressId m.tagSummaries

        net td =
            case td of
                HasTagSummary tagdata ->
                    (if tagdata.broadCategory == "exchange" then
                        Network.updateAddress addressId
                            (s_exchange tagdata.bestLabel)
                            m.network

                     else
                        m.network
                    )
                        |> Network.updateAddress addressId (s_hasTags (tagdata.tagCount > 0 && not (hasOnlyExchangeTags tagdata)))
                        |> Network.updateAddress addressId (s_hasActor (tagdata.bestActor /= Nothing))

                HasTags ->
                    Network.updateAddress addressId (s_hasTags True) m.network

                _ ->
                    m.network
    in
    tag |> Maybe.map (\n -> { m | network = net n }) |> Maybe.withDefault m


getNextTxEffects : Model -> Id -> Direction -> List Effect
getNextTxEffects model addressId direction =
    let
        context =
            { addressId = addressId
            , direction = direction
            , hops = 0
            }
    in
    Network.getRecentTxForAddress model.network (Direction.flip direction) addressId
        |> Maybe.map
            (\tx ->
                case tx.type_ of
                    Tx.Account t ->
                        BrowserGotBlockHeight
                            >> WorkflowNextTxByTime context
                            |> Api.GetBlockByDateEffect
                                { currency = t.raw.network
                                , datetime =
                                    t.raw.timestamp
                                        |> timestampToPosix
                                }
                            |> ApiEffect

                    Tx.Utxo t ->
                        WorkflowNextUtxoTx.loadReferencedTx context t.raw
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
                |> ApiEffect
            )
        |> List.singleton


updateByRoute : Plugins -> Update.Config -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute plugins uc route model =
    forcePushHistory (model |> s_isDirty True)
        |> updateByRoute_ plugins uc route


updateByRoute_ : Plugins -> Update.Config -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute_ plugins uc route model =
    case route |> Log.log "route" of
        Route.Root ->
            unselect model
                |> n

        Route.Network network (Route.Address a) ->
            let
                id =
                    Id.init network a

                ( x, b ) =
                    { model | network = Network.clearSelection model.network }
                        |> loadAddress plugins id
                        |> and (selectAddress uc id)
            in
            ( x, b )

        Route.Network network (Route.Tx a) ->
            let
                id =
                    Id.init network a

                m1 =
                    { model | network = Network.clearSelection model.network }
            in
            loadTx plugins id m1
                |> and (selectTx id)

        _ ->
            n model


loadAddress : Plugins -> Id -> Model -> ( Model, List Effect )
loadAddress =
    loadAddressWithPosition Auto


loadAddressWithPosition : FindPosition -> Plugins -> Id -> Model -> ( Model, List Effect )
loadAddressWithPosition position _ id model =
    if Dict.member id model.network.addresses then
        n model

    else
        let
            nw =
                Network.addAddressWithPosition position id model.network
        in
        ( { model | network = nw } |> updateTagDataOnAddress id
        , BrowserGotAddressData id
            |> Api.GetAddressEffect
                { currency = Id.network id
                , address = Id.id id
                , includeActors = True
                }
            |> ApiEffect
            |> List.singleton
        )


loadTx : Plugins -> Id -> Model -> ( Model, List Effect )
loadTx _ id model =
    if Dict.member id model.network.txs then
        n model

    else
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

                m1 =
                    unselect model
                        |> s_details (TxDetails.init tx |> TxDetails id |> Just)
            in
            selectedTx
                |> Maybe.map (\a -> Network.updateTx a (s_selected False) m1.network)
                |> Maybe.withDefault m1.network
                |> Network.updateTx id (s_selected True)
                |> flip s_network m1
                |> s_selection (SelectedTx id)
                |> bulkfetchTagsForTx tx

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
                        |> RemoteData.map (AddressDetails.init model.network uc.locale address.id)

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

                m1 =
                    unselect model
                        |> s_details
                            (RemoteData.map first details
                                |> AddressDetails id
                                |> Just
                            )
            in
            Network.updateAddress id (s_selected True) m1.network
                |> flip s_network m1
                |> s_selection (SelectedAddress id)
                |> openAddressTransactionsTable
                |> mapSecond ((++) eff)

        Nothing ->
            s_selection (WillSelectAddress id) model
                |> n


unselect : Model -> Model
unselect model =
    let
        network =
            case model.selection of
                SelectedAddress a ->
                    Network.updateAddress a (s_selected False) model.network

                SelectedTx a ->
                    Network.updateTx a (s_selected False) model.network

                _ ->
                    model.network
    in
    network
        |> flip s_network model
        |> s_details Nothing
        |> s_tooltip Nothing
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


bulkfetchTagsForTx : Tx.Tx -> Model -> ( Model, List Effect )
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

                addr =
                    addresses raw.inputs
                        ++ addresses raw.outputs
                        |> Set.fromList
                        |> Set.toList
            in
            ( { model
                | tagSummaries =
                    addr
                        |> List.foldl
                            (\a -> Dict.insert a LoadingTags)
                            model.tagSummaries
              }
            , List.Extra.greedyGroupsOf 100 addr
                |> List.map
                    (\adr ->
                        BrowserGotAddressesTags adr
                            |> Api.BulkGetAddressTagsEffect
                                { currency = raw.currency
                                , addresses = List.map Id.id adr
                                , pagesize = Just 1
                                , includeBestClusterTag = True
                                }
                            |> ApiEffect
                    )
            )

        _ ->
            n model


fetchTagSummaryForId : Dict.Dict Id HavingTags -> Id -> Effect
fetchTagSummaryForId existing id =
    case Dict.get id existing of
        Just (HasTagSummary _) ->
            CmdEffect Cmd.none

        _ ->
            BrowserGotTagSummary id
                |> Api.GetAddressTagSummaryEffect { currency = Id.network id, address = Id.id id, includeBestClusterTag = True }
                |> ApiEffect


fetchActorsForAddress : Api.Data.Address -> Dict.Dict String Api.Data.Actor -> List Effect
fetchActorsForAddress d existing =
    d.actors
        |> Maybe.map (List.filter (\l -> not (Dict.member l.id existing)))
        |> Maybe.map (List.map (.id >> fetchActor))
        |> Maybe.withDefault []


fetchActors : List String -> Dict.Dict String Api.Data.Actor -> List Effect
fetchActors d existing =
    d
        |> List.filter (\l -> not (Dict.member l existing))
        |> List.map fetchActor


getBiggestIO : Maybe (List Api.Data.TxValue) -> Maybe String -> Maybe String
getBiggestIO io exceptAddress =
    Maybe.withDefault [] io
        |> (exceptAddress
                |> Maybe.map
                    (\a -> List.filter (.address >> List.all ((/=) a)))
                |> Maybe.withDefault identity
           )
        |> List.sortBy (.value >> .value)
        |> List.reverse
        |> List.head
        |> Maybe.map .address
        |> Maybe.andThen List.head


getAddressForDirection : Tx -> Direction -> Maybe String -> Maybe Id
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
                    Just raw.fromAddress

                Outgoing ->
                    Just raw.toAddress
            )
                |> Maybe.map (Id.init raw.network)


addTx : Plugins -> Update.Config -> Id -> Direction -> Api.Data.Tx -> Model -> ( Model, List Effect )
addTx plugins _ addressId direction tx model =
    let
        ( newTx, network ) =
            Network.addTxWithPosition (Network.NextTo ( direction, addressId )) tx model.network

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
            Id.id addressId

        -- TODO what if multisig?
        firstAddress =
            getAddressForDirection newTx direction (Just address) |> Maybe.map Id.id
    in
    firstAddress
        |> Maybe.map
            (\a ->
                let
                    position =
                        NextTo ( direction, newTx.id )
                in
                loadAddressWithPosition position plugins (Id.init (Id.network addressId) a) newmodel
            )
        |> Maybe.withDefault (n newmodel)


checkSelection : Update.Config -> Model -> ( Model, List Effect )
checkSelection uc model =
    case model.selection of
        WillSelectTx id ->
            selectTx id model

        WillSelectAddress id ->
            selectAddress uc id model

        _ ->
            n model


openAddressTransactionsTable : Model -> ( Model, List Effect )
openAddressTransactionsTable model =
    case model.details of
        Just (AddressDetails id (Success ad)) ->
            let
                ( new, eff ) =
                    AddressDetails.showTransactionsTable ad True
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
        , tooltip = Nothing
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


multiSelect : Model -> List MultiSelectOptions -> Bool -> ( Model, List Effect )
multiSelect m sel keepOld =
    let
        newSelection =
            case m.selection of
                MultiSelect x ->
                    if keepOld then
                        x ++ sel

                    else
                        sel

                SelectedAddress oid ->
                    MSelectedAddress oid :: sel

                SelectedTx oid ->
                    MSelectedTx oid :: sel

                _ ->
                    sel

        selectItem s item n =
            case item of
                MSelectedAddress id ->
                    Network.updateAddress id (s_selected s) n

                MSelectedTx id ->
                    Network.updateTx id (s_selected s) n

        nNet =
            List.foldl (selectItem True) (Network.clearSelection m.network) newSelection
    in
    ( { m | selection = MultiSelect newSelection, network = nNet }, [] )


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


fromDeserialized : Deserialized -> Model -> ( Model, List Effect )
fromDeserialized deserialized model =
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
    in
    ( { model
        | network = ingestAddresses Network.init deserialized.addresses
        , history = History.init
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
            if List.any ((==) Incoming) addresses then
                Nothing

            else
                getAddressForDirection tx Incoming Nothing

        dst =
            if List.any ((==) Outgoing) addresses then
                Nothing

            else
                (src |> Maybe.map Id.id)
                    |> getAddressForDirection tx Outgoing
    in
    [ src, dst ]
        |> List.filterMap identity
        |> List.foldl aggAddressAdd (n model)
