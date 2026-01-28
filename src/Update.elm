module Update exposing (update, updateByPluginOutMsg, updateByUrl)

import Api
import Api.Data
import Basics.Extra exposing (flip)
import Browser
import Browser.Dom
import Config
import Config.Update exposing (Config)
import Dict exposing (Dict)
import Effect.Api
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Pathfinder as Pathfinder
import Encode.Graph as Graph
import Encode.Pathfinder as Pathfinder
import File.Download
import Hovercard
import Http exposing (Error(..))
import Init.Graph
import Init.Pathfinder
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Table.TagsTable as TagsTable
import Init.Pathfinder.Tooltip as Tooltip
import Init.Search as Search
import Json.Decode
import Json.Encode exposing (Value)
import List.Extra
import Log
import Maybe.Extra
import Model exposing (..)
import Model.Address as Address
import Model.Dialog as Dialog
import Model.Entity as Entity
import Model.Graph.Coords exposing (BBox)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Model.Locale as Locale
import Model.Notification as Notification exposing (Notification)
import Model.Pathfinder.Error exposing (Error(..))
import Model.Pathfinder.Tooltip as Tooltip
import Model.Search as Search
import Model.Statusbar as Statusbar
import Msg.Graph as Graph
import Msg.Locale as LocaleMsg
import Msg.Pathfinder as Pathfinder
import Msg.Search as Search
import Plugin
import Plugin.Msg as Plugin
import Plugin.Update as Plugin exposing (Plugins)
import PluginInterface.Msg as PluginInterface
import PluginInterface.Update as PluginInterface
import Ports
import Process
import RecordSetter exposing (..)
import RemoteData as RD
import Result.Extra
import Route
import Route.Graph
import Route.Pathfinder
import Set
import Sha256
import Task
import Time
import Tuple exposing (..)
import Update.Dialog as Dialog
import Update.Graph as Graph
import Update.Locale as Locale
import Update.Notification as Notification
import Update.Pathfinder as Pathfinder
import Update.Pathfinder.AddTagDialog as AddTagDialog
import Update.Search as Search
import Update.Statusbar as Statusbar
import Url exposing (Url)
import Util exposing (n)
import Util.Http exposing (Headers)
import Util.ThemedSelectBox as TSelectBox
import View.Locale as Locale exposing (makeTimestampFilename)
import View.Pathfinder.Legend exposing (legendView)
import Yaml.Decode


setConcepts : List Api.Data.Concept -> Model t -> Model t
setConcepts concepts model =
    { model
        | config =
            model.config
                |> s_allConcepts concepts
    }


setAbuseConcepts : List Api.Data.Concept -> Model t -> Model t
setAbuseConcepts concepts model =
    { model
        | config =
            model.config
                |> s_abuseConcepts concepts
    }


delay : Float -> msg -> Cmd msg
delay time msg =
    -- create a task that sleeps for `time`
    Process.sleep time
        |> -- once the sleep is over, ignore its output (using `always`)
           -- and then we create a new task that simply returns a success, and the msg
           Task.map (always <| msg)
        |> -- finally, we ask Elm to perform the Task, which
           -- takes the result of the above task and
           -- returns it to our update function
           Task.perform identity


tooltipBeginClosing : Msg -> Bool -> ( Model key, List Effect ) -> ( Model key, List Effect )
tooltipBeginClosing closingMsg withDelay ( model, eff ) =
    ( { model | tooltip = model.tooltip |> Maybe.map (s_closing True) }
    , ((delay
            (if withDelay then
                500.0

             else
                0
            )
        <|
            closingMsg
       )
        |> CmdEffect
      )
        :: eff
    )


tooltipAbortClosing : ( Model key, List Effect ) -> ( Model key, List Effect )
tooltipAbortClosing ( model, eff ) =
    ( { model | tooltip = model.tooltip |> Maybe.map (s_closing False) }, eff )


tooltipCloseIfNotAborted : ( Model key, List Effect ) -> ( Model key, List Effect )
tooltipCloseIfNotAborted ( model, eff ) =
    ( { model
        | tooltip =
            case model.tooltip of
                Just { closing } ->
                    if closing then
                        Nothing

                    else
                        model.tooltip

                _ ->
                    model.tooltip
      }
    , eff
    )


update : Plugins -> Config -> Msg -> Model key -> ( Model key, List Effect )
update plugins uc msg model =
    case Log.log "msg" msg of
        NoOp ->
            n model

        UserClosesNavbarSubMenu ->
            n { model | navbarSubMenu = Nothing }

        UserToggledNavbarSubMenu t ->
            n
                { model
                    | navbarSubMenu =
                        case model.navbarSubMenu of
                            Just _ ->
                                Nothing

                            Nothing ->
                                Just { type_ = t }
                }

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

        OpenTooltip ctx tttype ->
            let
                ( hc, cmd ) =
                    ctx.domId |> Hovercard.init

                tt =
                    tttype |> Tooltip.init hc

                ( hasToChange, newTooltip ) =
                    ( model.tooltip
                        |> Maybe.map (Tooltip.isSameTooltip tt >> not)
                        |> Maybe.withDefault True
                    , Just tt
                    )

                closing =
                    model.tooltip
                        |> Maybe.map .closing
                        |> Maybe.withDefault False

                open =
                    model.tooltip
                        |> Maybe.map .open
                        |> Maybe.withDefault False
            in
            if not hasToChange && not closing && not open then
                ( { model | tooltip = newTooltip |> Maybe.map (s_open True) }
                , Cmd.map HovercardMsg cmd
                    |> CmdEffect
                    |> List.singleton
                )

            else
                n model

        OpeningTooltip ctx withDelay tttype ->
            let
                ( hc, _ ) =
                    ctx.domId |> Hovercard.init

                tt =
                    tttype |> Tooltip.init hc

                ( hasToChange, newTooltip ) =
                    ( model.tooltip
                        |> Maybe.map (Tooltip.isSameTooltip tt >> not)
                        |> Maybe.withDefault True
                    , Just tt
                    )

                openDelay =
                    delay
                        (if withDelay then
                            2000.0

                         else
                            0.0
                        )
                        (OpenTooltip ctx tttype)
            in
            if hasToChange then
                ( { model | tooltip = newTooltip }
                , openDelay |> CmdEffect |> List.singleton
                )

            else
                n model |> tooltipAbortClosing

        ClosingTooltip ctx withDelay ->
            case model.tooltip of
                Just tt ->
                    n model |> tooltipBeginClosing (CloseTooltip ctx tt.type_) withDelay

                _ ->
                    n model

        RepositionTooltip ->
            ( model
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

        CloseTooltip ctx _ ->
            let
                ( nm, eff ) =
                    model |> n |> tooltipCloseIfNotAborted

                ( newPluginsState, outMsg, cmdp ) =
                    if model.tooltip /= nm.tooltip then
                        PluginInterface.ClosedTooltip ctx
                            |> Plugin.updateByCoreMsg plugins uc nm.plugins

                    else
                        ( model.plugins, [], Cmd.none )
            in
            (( nm, eff ) |> Tuple.mapFirst (s_plugins newPluginsState))
                |> updateByPluginOutMsg plugins uc outMsg
                |> Tuple.mapSecond ((++) [ PluginEffect cmdp ])

        UserRequestsUrl request ->
            case request of
                Browser.Internal url ->
                    ( model |> s_navbarSubMenu Nothing
                    , Url.toString url
                        |> NavPushUrlEffect
                        |> List.singleton
                    )

                Browser.External url ->
                    ( model |> s_navbarSubMenu Nothing
                    , NavLoadEffect url
                        |> List.singleton
                    )

        BrowserChangedUrl url ->
            updateByUrl plugins uc url model

        RuntimePostponedUpdateByUrl url ->
            updateByUrl plugins uc url model

        BrowserGotStatistics stats ->
            let
                ( newPluginsState, outMsg, cmd ) =
                    PluginInterface.CoreGotStatsUpdate stats
                        |> Plugin.updateByCoreMsg plugins uc model.plugins
            in
            n
                { model
                    | stats = RD.Success stats
                    , statusbar = Statusbar.updateLastBlocks stats model.statusbar
                    , search =
                        if model.page == Graph then
                            model.search
                                |> s_searchType
                                    (Search.initSearchAll (Just stats))

                        else
                            model.search
                                |> s_searchType
                                    (Search.initSearchAddressAndTxs Nothing)
                    , plugins = newPluginsState
                }
                |> Tuple.mapSecond ((::) (PluginEffect cmd))
                |> updateByPluginOutMsg plugins uc outMsg

        -- Plugin handling
        BrowserGotEntityTaxonomy concepts ->
            setConcepts concepts model
                |> n

        BrowserGotAbuseTaxonomy concepts ->
            setAbuseConcepts concepts model
                |> n

        BrowserGotSupportedTokens currency configs ->
            let
                locale =
                    Locale.setSupportedTokens configs currency model.config.locale
            in
            { model
                | supportedTokens = Dict.insert currency configs model.supportedTokens
                , config =
                    model.config
                        |> s_locale locale
            }
                |> n

        BrowserCancelledRequest statusbarToken ->
            n
                { model
                    | statusbar = Statusbar.update statusbarToken Nothing model.statusbar
                }

        BrowserGotResponseWithHeaders statusbarToken result ->
            let
                notFound token =
                    Statusbar.getMessage token model.statusbar
                        |> Maybe.andThen
                            (\( key, v ) ->
                                if key == Statusbar.loadingAddressKey || key == Statusbar.loadingAddressEntityKey || key == Statusbar.loadingTransactionKey then
                                    List.Extra.getAt 0 v
                                        |> Maybe.map
                                            (\thing ->
                                                let
                                                    notFoundError =
                                                        if key == Statusbar.loadingTransactionKey then
                                                            Dialog.txNotFoundError

                                                        else
                                                            Dialog.addressNotFoundError
                                                in
                                                UserClosesDialog
                                                    |> notFoundError thing model.dialog
                                            )

                                else
                                    Nothing
                            )

                newDialog =
                    case result of
                        Err ( Http.BadStatus 401, _, _ ) ->
                            UserClosesDialog
                                |> Dialog.generalError
                                    { title = "Session expired"
                                    , message = "popup-session-expired-info"
                                    , variables = []
                                    }
                                |> Just

                        Err e ->
                            statusbarToken
                                |> Maybe.andThen
                                    (\token ->
                                        case e of
                                            ( Http.BadStatus 404, _, _ ) ->
                                                notFound token

                                            ( Http.BadStatus 400, _, _ ) ->
                                                notFound token

                                            _ ->
                                                model.dialog
                                    )
                                |> Maybe.Extra.orElse model.dialog

                        _ ->
                            model.dialog

                isErrorDialogShown =
                    case newDialog of
                        Just (Dialog.Error _) ->
                            True

                        _ ->
                            False

                ( notifications, notificationEffects ) =
                    case ( isErrorDialogShown, result ) of
                        ( False, Err ( httpErr, _, _ ) ) ->
                            case httpErr of
                                Http.BadStatus 429 ->
                                    Notification.add
                                        (apiRateExceededError model.config.locale model.user.auth)
                                        model.notifications

                                _ ->
                                    Notification.addHttpError model.notifications Nothing httpErr

                        _ ->
                            n model.notifications
            in
            { model
                | statusbar =
                    case statusbarToken of
                        Just t ->
                            Statusbar.update
                                t
                                (case result of
                                    Err ( Http.BadStatus 401, _, _ ) ->
                                        Nothing

                                    Err ( err, _, _ ) ->
                                        Just err

                                    Ok _ ->
                                        Nothing
                                )
                                model.statusbar

                        Nothing ->
                            case result of
                                Err ( Http.BadStatus 429, _, _ ) ->
                                    Just (Http.BadStatus 429)
                                        |> Statusbar.add model.statusbar "search" []

                                _ ->
                                    model.statusbar
                , dialog = newDialog
                , notifications = notifications
            }
                |> handleResponse plugins
                    uc
                    result
                |> mapSecond ((++) (List.map NotificationEffect notificationEffects))

        UserClosesDialog ->
            case model.dialog of
                Just (Dialog.Error _) ->
                    n { model | dialog = Nothing }

                Just (Dialog.TagsList _) ->
                    n { model | dialog = Nothing, tooltip = Nothing, navbarSubMenu = Nothing }

                _ ->
                    n { model | dialog = Nothing }

        TagsListDialogTableUpdateMsg tableState ->
            case model.dialog of
                Just (Dialog.TagsList config) ->
                    let
                        newConfig =
                            { config | tagsTable = config.tagsTable |> s_state tableState }
                    in
                    n { model | dialog = Just (Dialog.TagsList newConfig) }

                _ ->
                    n model

        UserClickedUserIcon id ->
            if model.user.hovercard == Nothing then
                let
                    ( hovercard, cmd ) =
                        Hovercard.init id
                in
                ( { model
                    | user =
                        model.user
                            |> s_hovercard (Just hovercard)
                  }
                , Cmd.map UserHovercardMsg cmd
                    |> CmdEffect
                    |> List.singleton
                )

            else
                n { model | user = model.user |> s_hovercard Nothing }

        UserHovercardMsg hm ->
            model.user.hovercard
                |> Maybe.map
                    (\hovercard ->
                        let
                            ( hovercard_, cmd ) =
                                Hovercard.update hm hovercard
                        in
                        ( { model
                            | user =
                                model.user
                                    |> s_hovercard (Just hovercard_)
                          }
                        , Cmd.map UserHovercardMsg cmd
                            |> CmdEffect
                            |> List.singleton
                        )
                    )
                |> Maybe.withDefault (n model)

        UserLeftUserHovercard ->
            { model
                | user =
                    case model.user.auth of
                        Unauthorized _ _ ->
                            model.user

                        _ ->
                            model.user |> s_hovercard Nothing
            }
                |> n

        UserSwitchesLocale loc ->
            switchLocale loc model

        UserClickedLightmode ->
            let
                newModel =
                    { model
                        | config =
                            model.config
                                |> s_lightmode (not model.config.lightmode)
                    }
            in
            ( newModel, [ saveUserSettings newModel ] )

        UserInputsApiKeyForm input ->
            { model
                | user =
                    model.user
                        |> s_apiKey input
            }
                |> n

        UserSubmitsApiKeyForm ->
            if String.isEmpty model.user.apiKey then
                n model

            else
                let
                    effs =
                        case model.user.auth of
                            Unauthorized _ effects ->
                                List.map ApiEffect effects

                            _ ->
                                []

                    ( new, outMsg, cmd ) =
                        Plugin.updateApiKeyHash plugins (Sha256.sha256 model.user.apiKey) model.plugins
                            |> PluginInterface.andThen (Plugin.updateApiKey plugins model.user.apiKey)
                in
                ( { model
                    | user =
                        model.user
                            |> s_auth
                                (if List.isEmpty effs then
                                    Unknown

                                 else
                                    Unauthorized True []
                                )
                            |> s_hovercard
                                (if List.isEmpty effs then
                                    Nothing

                                 else
                                    model.user.hovercard
                                )
                    , plugins = new
                  }
                , PluginEffect cmd
                    :: effs
                )
                    |> updateByPluginOutMsg plugins uc outMsg

        BrowserGotContentsElement result ->
            result
                |> Result.map
                    (\{ element } ->
                        { model
                            | config =
                                model.config
                                    |> s_size
                                        (Just
                                            { width = element.width
                                            , height = element.height
                                            , x = element.x
                                            , y = element.y
                                            }
                                        )
                        }
                    )
                |> Result.withDefault model
                |> n

        BrowserChangedWindowSize w h ->
            { model
                | width = w
                , height = h
                , config = updateSize (w - model.width) (h - model.height) model.config
            }
                |> n

        UserClickedStatusbar ->
            { model
                | statusbar = Statusbar.toggle model.statusbar
            }
                |> n

        UserClickedLayout ->
            let
                ( new, outMsg, cmd ) =
                    PluginInterface.ClickedOnNeutralGround
                        |> Plugin.updateByCoreMsg plugins uc model.plugins
            in
            clearSearch plugins
                { model
                    | user =
                        model.user
                            |> s_hovercard Nothing
                    , plugins = new
                    , pathfinder = model.pathfinder |> s_contextMenu Nothing |> s_helpDropdownOpen False
                    , navbarSubMenu = Nothing
                }
                |> Tuple.mapSecond ((::) (PluginEffect cmd))
                |> updateByPluginOutMsg plugins uc outMsg

        UserClickedNavBack ->
            ( model, NavBackEffect |> List.singleton )

        UserClickedNavHome ->
            ( model, NavPushUrlEffect "/" |> List.singleton )

        TimeUpdateReset _ ->
            { model
                | user =
                    model.user
                        |> s_auth
                            (case model.user.auth of
                                Authorized auth ->
                                    Authorized
                                        { auth
                                            | requestLimit =
                                                case auth.requestLimit of
                                                    Limited rl ->
                                                        let
                                                            reset =
                                                                max 0 <| rl.reset - 1
                                                        in
                                                        Limited
                                                            { rl
                                                                | reset = reset
                                                                , remaining =
                                                                    if reset == 0 then
                                                                        rl.limit

                                                                    else
                                                                        rl.remaining
                                                            }

                                                    _ ->
                                                        auth.requestLimit
                                        }

                                _ ->
                                    model.user.auth
                            )
            }
                |> n

        UserClickedExampleSearch str ->
            let
                ( search, eff ) =
                    Search.setQuery str model.search
                        |> s_visible True
                        |> Search.triggerSearch str
            in
            update plugins uc (Search.UserFocusSearch |> SearchMsg) model
                |> mapFirst (s_search search)
                |> mapSecond
                    ((++)
                        (eff |> List.map (SearchEffect SearchMsg))
                    )

        UserClickedLogout ->
            let
                ( new, outMsg, cmd ) =
                    Plugin.logout plugins model.plugins
            in
            ( { model
                | plugins = new
                , user =
                    model.user
                        |> s_auth
                            (case model.user.auth of
                                Authorized auth ->
                                    Authorized
                                        { auth | loggingOut = True }

                                _ ->
                                    model.user.auth
                            )
              }
            , [ PluginEffect cmd
              , LogoutEffect
              ]
            )
                |> updateByPluginOutMsg plugins uc outMsg

        BrowserGotElementForPlugin pmsg element ->
            updatePlugins plugins uc (pmsg element) model

        LocaleMsg m ->
            let
                ( locale, localeEffects ) =
                    Locale.update m model.config.locale

                newModel =
                    { model
                        | config =
                            model.config
                                |> s_locale locale
                    }

                eff =
                    case m of
                        LocaleMsg.BrowserSentTimezone _ ->
                            [ saveUserSettings newModel ]

                        _ ->
                            []
            in
            ( newModel
            , List.map LocaleEffect localeEffects ++ eff
            )

        SettingsMsg (UserChangedPreferredCurrency currency) ->
            let
                newModel =
                    { model
                        | config =
                            model.config
                                |> s_preferredFiatCurrency currency
                    }
            in
            ( newModel, [ saveUserSettings newModel ] )

        SettingsMsg UserToggledValueDisplay ->
            let
                showInFiat =
                    not model.config.showValuesInFiat

                newModel =
                    { model
                        | config =
                            model.config
                                |> s_showValuesInFiat showInFiat
                    }
            in
            ( newModel, [ saveUserSettings newModel ] )

        SettingsMsg UserToggledBothValueDisplay ->
            let
                showBoth =
                    not model.config.showBothValues

                newModel =
                    { model
                        | config =
                            model.config
                                |> s_showBothValues showBoth
                    }
            in
            ( newModel, [ saveUserSettings newModel ] )

        AddTagDialog smsg ->
            case model.dialog of
                Just (Dialog.AddTag conf) ->
                    let
                        ( nm, eff ) =
                            AddTagDialog.update uc smsg conf
                    in
                    case smsg of
                        BrowserAddedTag _ ->
                            ( model |> s_dialog Nothing, eff )

                        _ ->
                            ( model |> s_dialog (Just (Dialog.AddTag nm))
                            , eff
                            )

                _ ->
                    n model

        SearchMsg m ->
            case m of
                Search.PluginMsg ms ->
                    updatePlugins plugins uc ms model

                Search.UserClicksResultLine ->
                    let
                        query =
                            Search.query model.search

                        selectedValue =
                            Search.selectedValue model.search
                                |> Maybe.Extra.orElse (Search.firstResult model.search)

                        ( search, _ ) =
                            Search.update m model.search

                        m2 =
                            { model | search = search }

                        resultLineToRoute v =
                            case ( model.page, v ) of
                                ( Pathfinder, Search.Address currency address ) ->
                                    Route.Pathfinder.addressRoute
                                        { network = currency
                                        , address = address
                                        }
                                        |> Route.pathfinderRoute

                                ( Pathfinder, Search.Tx currency tx ) ->
                                    Route.Pathfinder.txRoute
                                        { network = currency
                                        , txHash = tx
                                        }
                                        |> Route.pathfinderRoute

                                ( Home, Search.Address currency address ) ->
                                    Route.Pathfinder.addressRoute
                                        { network = currency
                                        , address = address
                                        }
                                        |> Route.pathfinderRoute

                                ( Home, Search.Tx currency tx ) ->
                                    Route.Pathfinder.txRoute
                                        { network = currency
                                        , txHash = tx
                                        }
                                        |> Route.pathfinderRoute

                                ( _, s ) ->
                                    Route.Graph.resultLineToRoute s
                                        |> Route.graphRoute
                    in
                    if String.isEmpty query then
                        n model

                    else
                        case selectedValue of
                            Just value ->
                                value
                                    |> resultLineToRoute
                                    |> Route.toUrl
                                    |> NavPushUrlEffect
                                    |> List.singleton
                                    |> pair m2

                            Nothing ->
                                model.stats
                                    |> RD.map
                                        (\stats ->
                                            { m2
                                                | dialog =
                                                    { message = Locale.string model.config.locale "Please-choose-ledger"
                                                    , options =
                                                        stats.currencies
                                                            |> List.map .name
                                                            |> List.map
                                                                (\name ->
                                                                    ( String.toUpper name
                                                                    , Search.UserPicksCurrency name |> SearchMsg
                                                                    )
                                                                )
                                                    , onClose = SearchMsg Search.UserClickedCloseCurrencyPicker
                                                    }
                                                        |> Dialog.options
                                                        |> Just
                                                , search =
                                                    Search.setIsPickingCurrency search
                                                        -- add back the query for UserPicksCurrency
                                                        |> Search.setQuery query
                                            }
                                        )
                                    |> RD.withDefault model
                                    |> n

                Search.UserClickedCloseCurrencyPicker ->
                    clearSearch plugins { model | dialog = Nothing }

                Search.UserPicksCurrency currency ->
                    let
                        ( graph, graphEffects ) =
                            case model.page of
                                Graph ->
                                    Graph.loadAddressPath plugins
                                        { currency = currency
                                        , addresses =
                                            Search.query model.search
                                                |> Search.getMulti
                                        }
                                        model.graph
                                        |> mapSecond (List.map GraphEffect)
                                        |> mapSecond
                                            ((++)
                                                [ Route.Graph.Root
                                                    |> Route.graphRoute
                                                    |> Route.toUrl
                                                    |> NavPushUrlEffect
                                                ]
                                            )

                                _ ->
                                    n model.graph

                        pathfinderEffects =
                            case model.page of
                                Graph ->
                                    []

                                _ ->
                                    Search.query model.search
                                        |> Search.getMulti
                                        |> List.Extra.uncons
                                        |> Maybe.map
                                            (\( fst, rest ) ->
                                                rest
                                                    |> List.Extra.uncons
                                                    |> Maybe.map
                                                        (\( snd, rest2 ) ->
                                                            fst
                                                                :: snd
                                                                :: rest2
                                                                |> List.map (Route.Pathfinder.AddressHop Route.Pathfinder.NormalAddress)
                                                                |> Route.Pathfinder.Path currency
                                                        )
                                                    |> Maybe.withDefault
                                                        (Route.Pathfinder.Address fst Nothing
                                                            |> Route.Pathfinder.Network currency
                                                        )
                                            )
                                        |> Maybe.map
                                            (Route.pathfinderRoute
                                                >> Route.toUrl
                                                >> NavPushUrlEffect
                                                >> List.singleton
                                            )
                                        |> Maybe.withDefault []

                        ( search, searchEffects ) =
                            Search.update m model.search
                    in
                    clearSearch plugins { model | graph = graph, search = search, dialog = Nothing }
                        |> mapSecond ((++) graphEffects)
                        |> mapSecond ((++) pathfinderEffects)
                        |> mapSecond ((++) (List.map (SearchEffect SearchMsg) searchEffects))

                _ ->
                    let
                        ( search, searchEffects ) =
                            Search.update m model.search
                    in
                    ( { model | search = search }
                    , List.map (SearchEffect SearchMsg) searchEffects
                    )

        PathfinderMsg Pathfinder.UserClickedShowLegend ->
            let
                closemsg =
                    UserClosesDialog

                viewPlugins =
                    Plugin.viewPlugins Config.plugins
            in
            n
                { model
                    | dialog =
                        Just
                            ({ html = legendView viewPlugins model.config closemsg
                             , defaultMsg = closemsg
                             }
                                |> Dialog.Custom
                            )
                }

        PathfinderMsg Pathfinder.UserClickedRestart ->
            if model.pathfinder.isDirty then
                n
                    { model
                        | dialog =
                            { message = Locale.string model.config.locale "Note-not-recoverable"
                            , confirmText = Just "Confirm-delete"
                            , cancelText = Just "Cancel"
                            , title = "Confirm-clear-dashboard"
                            , onYes = PathfinderMsg Pathfinder.UserClickedRestartYes
                            , onNo = NoOp
                            }
                                |> Dialog.confirm
                                |> Just
                    }

            else
                n model

        PathfinderMsg Pathfinder.UserClickedRestartYes ->
            let
                ( m, cmd ) =
                    Init.Pathfinder.init (Model.userSettingsFromMainModel model)

                ( newPluginsState, outMsg, cmdp ) =
                    PluginInterface.Reset
                        |> Plugin.updateByCoreMsg plugins uc model.plugins
            in
            ( { model | pathfinder = m, plugins = newPluginsState }, [ CmdEffect (cmd |> Cmd.map PathfinderMsg) ] )
                |> updateByPluginOutMsg plugins uc outMsg
                |> Tuple.mapSecond
                    ((++)
                        [ PluginEffect cmdp
                        , Route.Pathfinder.Root
                            |> Route.pathfinderRoute
                            |> Route.toUrl
                            |> NavPushUrlEffect
                        ]
                    )

        PathfinderMsg Pathfinder.UserClickedToggleTracingMode ->
            let
                ( pf, pfeff ) =
                    Pathfinder.update plugins uc Pathfinder.UserClickedToggleTracingMode model.pathfinder

                nm =
                    { model | pathfinder = pf }
            in
            ( nm
            , saveUserSettings nm :: List.map PathfinderEffect pfeff
            )

        PathfinderMsg (Pathfinder.ChangedDisplaySettingsMsg dsm) ->
            let
                ( pf, pfeff ) =
                    Pathfinder.update plugins uc (Pathfinder.ChangedDisplaySettingsMsg dsm) model.pathfinder

                ( nm, neff ) =
                    ( model |> s_pathfinder pf, pfeff |> List.map PathfinderEffect )
            in
            case dsm of
                Pathfinder.UserClickedToggleDatesInUserLocale ->
                    toggleShowDatesInUserLocale nm |> Tuple.mapSecond ((++) neff)

                Pathfinder.UserClickedToggleSnapToGrid ->
                    toggleSnapToGrid nm |> Tuple.mapSecond ((++) neff)

                Pathfinder.UserClickedToggleShowTimeZoneOffset ->
                    toggleShowTimeZoneOffset nm |> Tuple.mapSecond ((++) neff)

                Pathfinder.UserClickedToggleHighlightClusterFriends ->
                    toggleHighlightClusterFriends nm |> Tuple.mapSecond ((++) neff)

                Pathfinder.UserClickedToggleShowTxTimestamp ->
                    togglShowTimestampOnTxEdge nm |> Tuple.mapSecond ((++) neff)

                Pathfinder.UserClickedToggleDisplaySettings ->
                    ( nm, neff )

                Pathfinder.UserClickedToggleValueDetail ->
                    let
                        option =
                            case model.config.locale.valueDetail of
                                Locale.Exact ->
                                    "magnitude"

                                _ ->
                                    "exact"
                    in
                    update plugins uc (Graph.UserChangesValueDetail option |> GraphMsg) model

                Pathfinder.UserClickedToggleShowHash ->
                    let
                        showHash =
                            not model.config.showHash

                        newModel =
                            { model
                                | config =
                                    model.config
                                        |> s_showHash showHash
                            }
                    in
                    ( newModel, [ saveUserSettings newModel ] )

                Pathfinder.UserClickedToggleValueDisplay ->
                    update plugins uc (UserToggledValueDisplay |> SettingsMsg) model

                Pathfinder.UserClickedToggleBothValueDisplay ->
                    update plugins uc (UserToggledBothValueDisplay |> SettingsMsg) model

        PathfinderMsg (Pathfinder.UserClickedSaveGraph time) ->
            ( model
            , [ (case time of
                    Nothing ->
                        Time.now
                            |> Task.perform (Just >> Pathfinder.UserClickedSaveGraph)

                    Just t ->
                        Pathfinder.encode model.pathfinder
                            |> pair
                                (makeTimestampFilename model.config.locale t
                                    |> (\tt -> tt ++ ".gs")
                                )
                            |> Ports.serialize
                )
                    |> Pathfinder.CmdEffect
                    |> PathfinderEffect
              , SetCleanEffect
              ]
            )

        PathfinderMsg (Pathfinder.UserGotDataForTagsListDialog id tags) ->
            let
                ( pathfinder, eff ) =
                    Pathfinder.update plugins uc (Pathfinder.UserGotDataForTagsListDialog id tags) model.pathfinder

                closemsg =
                    UserClosesDialog
            in
            ( { model
                | pathfinder = pathfinder
                , dialog =
                    Just
                        (Dialog.TagsList
                            { tagsTable = TagsTable.init tags
                            , id = id
                            , closeMsg = closemsg
                            }
                        )
              }
            , List.map PathfinderEffect eff
            )

        PathfinderMsg (Pathfinder.UserOpensDialogWindow (Pathfinder.AddTags id)) ->
            n
                { model
                    | dialog =
                        Just
                            (Dialog.AddTag
                                { id = id
                                , closeMsg = UserClosesDialog
                                , addTagMsg = AddTagDialog (UserClickedAddTag id)
                                , search = Search.init Search.SearchActorsOnly
                                , selectedActor = Nothing
                                , description = ""
                                }
                            )
                }

        PathfinderMsg m ->
            let
                pathfinderOld =
                    model.pathfinder

                ( newModel, newEffects ) =
                    case m of
                        Pathfinder.UserClickedExportGraphAsImage name ->
                            ( model
                            , (name ++ ".png")
                                |> Ports.exportGraphImage
                                |> Pathfinder.CmdEffect
                                |> PathfinderEffect
                                |> List.singleton
                            )

                        Pathfinder.UserClickedExportGraphAsPdf name ->
                            let
                                ( nm, neff ) =
                                    Notification.add
                                        (Notification.infoDefault "generating pdf"
                                            -- |> Notification.map (s_title (Just "PDF Export"))
                                            |> Notification.map (s_isEphemeral True)
                                            |> Notification.map (s_showClose False)
                                            |> Notification.map (s_removeDelayMs 4000.0)
                                        )
                                        model.notifications
                            in
                            ( model |> s_notifications nm
                            , ((name ++ ".pdf")
                                |> Ports.exportGraphPdf
                                |> Pathfinder.CmdEffect
                                |> PathfinderEffect
                              )
                                :: List.map NotificationEffect neff
                            )

                        Pathfinder.UserReleasedEscape ->
                            let
                                ( pf, pfeff ) =
                                    Pathfinder.update plugins uc Pathfinder.UserReleasedEscape model.pathfinder

                                ( nm, neff ) =
                                    ( model |> s_pathfinder pf, pfeff |> List.map PathfinderEffect )
                            in
                            ( nm |> s_dialog Nothing |> s_notifications (nm.notifications |> Notification.pop), neff )

                        Pathfinder.BrowserGotBulkAddresses addresses ->
                            let
                                ( new, outMsg, cmd ) =
                                    addresses
                                        |> List.map (\x -> { address = x.address, currency = x.currency })
                                        |> PluginInterface.AddressesAdded
                                        |> Plugin.updateByCoreMsg plugins uc model.plugins

                                ( pathfinder, pathfinderEffects ) =
                                    Pathfinder.update plugins uc m model.pathfinder
                            in
                            ( { model
                                | plugins = new
                                , pathfinder = pathfinder
                              }
                            , PluginEffect cmd
                                :: List.map PathfinderEffect pathfinderEffects
                            )
                                |> updateByPluginOutMsg plugins uc outMsg

                        Pathfinder.InternalPathfinderAddedAddress addressId ->
                            let
                                ( new, outMsg, cmd ) =
                                    addressId
                                        |> Address.fromPathfinderId
                                        |> List.singleton
                                        |> PluginInterface.AddressesAdded
                                        |> Plugin.updateByCoreMsg plugins uc model.plugins

                                ( pathfinder, pathfinderEffects ) =
                                    Pathfinder.update plugins uc m model.pathfinder
                            in
                            ( { model
                                | plugins = new
                                , pathfinder = pathfinder
                              }
                            , PluginEffect cmd
                                :: List.map PathfinderEffect pathfinderEffects
                            )
                                |> updateByPluginOutMsg plugins uc outMsg

                        Pathfinder.BrowserGotClusterData _ data ->
                            let
                                ( new, outMsg, cmd ) =
                                    { currency = data.currency, entity = data.entity }
                                        |> List.singleton
                                        |> PluginInterface.EntitiesAdded
                                        |> Plugin.updateByCoreMsg plugins uc model.plugins

                                ( pathfinder, pathfinderEffects ) =
                                    Pathfinder.update plugins uc m model.pathfinder
                            in
                            ( { model
                                | plugins = new
                                , pathfinder = pathfinder
                              }
                            , PluginEffect cmd
                                :: List.map PathfinderEffect pathfinderEffects
                            )
                                |> updateByPluginOutMsg plugins uc outMsg

                        Pathfinder.PluginMsg ms ->
                            let
                                -- route plugin msgs through pathfinder
                                -- needed to handle things like undo/redo
                                ( pathfinder, eff ) =
                                    Pathfinder.update plugins uc m model.pathfinder

                                nm =
                                    { model | pathfinder = pathfinder }

                                neff =
                                    List.map PathfinderEffect eff
                            in
                            updatePlugins plugins uc ms nm |> Tuple.mapSecond ((++) neff)

                        _ ->
                            let
                                ( pathfinder, eff ) =
                                    Pathfinder.update plugins uc m model.pathfinder

                                nm =
                                    { model | pathfinder = pathfinder }
                            in
                            ( nm
                            , List.map PathfinderEffect eff
                            )
            in
            if newModel.pathfinder.network == pathfinderOld.network && newModel.pathfinder.annotations == pathfinderOld.annotations then
                ( newModel, newEffects )

            else
                let
                    ( newPluginsState, outMsg, cmd ) =
                        (PluginInterface.PathfinderGraphChanged |> PluginInterface.InMsgsPathfinder)
                            |> Plugin.updateByCoreMsg plugins uc model.plugins
                in
                ( { newModel | plugins = newPluginsState }, newEffects ++ [ PluginEffect cmd ] )
                    |> updateByPluginOutMsg plugins uc outMsg

        GraphMsg m ->
            case m of
                Graph.PluginMsg ms ->
                    updatePlugins plugins uc ms model

                Graph.InternalGraphAddedAddresses ids ->
                    let
                        ( new, outMsg, cmd ) =
                            ids
                                |> Set.toList
                                |> List.map Address.fromId
                                |> PluginInterface.AddressesAdded
                                |> Plugin.updateByCoreMsg plugins uc model.plugins

                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph
                    in
                    ( { model
                        | plugins = new
                        , graph = graph
                        , dirty = True
                      }
                    , PluginEffect cmd
                        :: SetDirtyEffect
                        :: List.map GraphEffect graphEffects
                    )
                        |> updateByPluginOutMsg plugins uc outMsg

                Graph.InternalGraphAddedEntities ids ->
                    let
                        ( new, outMsg, cmd ) =
                            ids
                                |> Set.toList
                                |> List.map Entity.fromId
                                |> PluginInterface.EntitiesAdded
                                |> Plugin.updateByCoreMsg plugins uc model.plugins

                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph
                    in
                    ( { model
                        | plugins = new
                        , graph = graph
                        , dirty = True
                      }
                    , PluginEffect cmd
                        :: SetDirtyEffect
                        :: List.map GraphEffect graphEffects
                    )
                        |> updateByPluginOutMsg plugins uc outMsg

                Graph.UserChangesCurrency currency ->
                    let
                        ( preferredCurrency, showFiat ) =
                            case currency of
                                "coin" ->
                                    ( model.config.preferredFiatCurrency
                                    , False
                                    )

                                fiat ->
                                    ( fiat
                                    , True
                                    )

                        newModel =
                            { model
                                | config =
                                    model.config
                                        |> s_preferredFiatCurrency preferredCurrency
                                        |> s_showValuesInFiat showFiat
                            }
                    in
                    ( newModel, [ saveUserSettings newModel ] )

                Graph.UserChangesValueDetail detail ->
                    let
                        locale =
                            Locale.changeValueDetail detail model.config.locale

                        newModel =
                            { model
                                | config =
                                    model.config
                                        |> s_locale locale
                            }
                    in
                    ( newModel, [ saveUserSettings newModel ] )

                Graph.UserClickedShowEntityShadowLinks ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, saveUserSettings newModel :: List.map GraphEffect graphEffects )

                Graph.UserClickedShowAddressShadowLinks ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, saveUserSettings newModel :: List.map GraphEffect graphEffects )

                Graph.UserClickedToggleShowZeroTransactions ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, saveUserSettings newModel :: List.map GraphEffect graphEffects )

                Graph.UserClickedToggleShowDatesInUserLocale ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        ( nm, neff ) =
                            ( model |> s_graph graph, graphEffects |> List.map GraphEffect )

                        ( newm, eff ) =
                            toggleShowDatesInUserLocale nm
                    in
                    ( newm, eff ++ neff )

                Graph.UserChangesAddressLabelType _ ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, saveUserSettings newModel :: List.map GraphEffect graphEffects )

                Graph.UserChangesTxLabelType _ ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, saveUserSettings newModel :: List.map GraphEffect graphEffects )

                Graph.UserClickedExportGS time ->
                    ( model
                    , [ (case time of
                            Nothing ->
                                Time.now
                                    |> Task.perform (Just >> Graph.UserClickedExportGS)

                            Just t ->
                                Graph.encode model.graph
                                    |> pair
                                        (makeTimestampFilename model.config.locale t
                                            |> (\tt -> tt ++ ".gs")
                                        )
                                    |> Ports.serialize
                        )
                            |> Graph.CmdEffect
                            |> GraphEffect
                      , SetCleanEffect
                      ]
                    )

                Graph.UserClickedExportGraphics time ->
                    ( model
                    , (case time of
                        Nothing ->
                            Time.now
                                |> Task.perform (Just >> Graph.UserClickedExportGraphics)

                        Just t ->
                            makeTimestampFilename model.config.locale t
                                |> (\tt -> tt ++ ".svg")
                                |> Ports.exportGraphics
                      )
                        |> Graph.CmdEffect
                        |> GraphEffect
                        |> List.singleton
                    )

                Graph.UserClickedExportTagPack time ->
                    ( model
                    , (case time of
                        Nothing ->
                            Time.now
                                |> Task.perform (Just >> Graph.UserClickedExportTagPack)

                        Just t ->
                            let
                                filename =
                                    makeTimestampFilename model.config.locale t
                                        |> (\tt -> tt ++ ".yaml")
                            in
                            Graph.makeTagPack model.graph t
                                |> File.Download.string filename "text/yaml"
                      )
                        |> Graph.CmdEffect
                        |> GraphEffect
                        |> List.singleton
                    )

                Graph.BrowserReadTagPackFile filename result ->
                    case result of
                        Err err ->
                            let
                                httpErr =
                                    Yaml.Decode.errorToString err
                                        |> Http.BadBody

                                ( notifications, notificationEffects ) =
                                    Notification.addHttpError model.notifications (Just filename) httpErr
                            in
                            ( { model
                                | statusbar =
                                    httpErr
                                        |> Just
                                        |> Statusbar.add model.statusbar filename []
                                , notifications = notifications
                              }
                            , List.map NotificationEffect notificationEffects
                            )

                        Ok yaml ->
                            let
                                -- Check which tags can be applied before importing
                                tagStats =
                                    Graph.checkTagsCanBeApplied yaml model.graph

                                updatedModel =
                                    { model
                                        | graph = Graph.importTagPack uc yaml model.graph
                                    }

                                -- Create notification if not all tags were applied
                                ( notifications, notificationEffects ) =
                                    if tagStats.applicableTags < tagStats.totalTags then
                                        let
                                            skippedCount =
                                                tagStats.totalTags - tagStats.applicableTags

                                            notification =
                                                Notification.infoDefault "tag-import-feedback"
                                                    |> Notification.map (s_title (Just "Tag Import"))
                                                    |> Notification.map
                                                        (s_variables
                                                            [ String.fromInt tagStats.applicableTags
                                                            , String.fromInt tagStats.totalTags
                                                            , String.fromInt skippedCount
                                                            ]
                                                        )
                                        in
                                        Notification.add notification model.notifications

                                    else
                                        n model.notifications
                            in
                            ( { updatedModel | notifications = notifications }
                            , List.map NotificationEffect notificationEffects
                            )

                Graph.PortDeserializedGS ( filename, data ) ->
                    pluginNewGraph plugins ( model, [] )
                        |> (\( mdl, eff ) ->
                                deserialize plugins uc filename data mdl
                                    |> mapSecond ((++) eff)
                           )

                Graph.UserClickedNew ->
                    if model.dirty then
                        { model
                            | dialog =
                                { message = Locale.string model.config.locale "Start-from-scratch"
                                , title = "Clear Graph?"
                                , confirmText = Nothing
                                , cancelText = Nothing
                                , onYes = GraphMsg Graph.UserClickedNewYes
                                , onNo = NoOp
                                }
                                    |> Dialog.confirm
                                    |> Just
                        }
                            |> n

                    else
                        n model

                Graph.UserClickedNewYes ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newGraph =
                            Time.posixToMillis graph.browser.now
                                |> Init.Graph.init (userSettingsFromMainModel model)
                                |> s_history graph.history
                                |> s_config
                                    (graph.config
                                        |> s_highlighter False
                                    )
                    in
                    ( { model
                        | graph = newGraph
                      }
                    , (Route.Graph.Root
                        |> Route.graphRoute
                        |> Route.toUrl
                        |> NavPushUrlEffect
                      )
                        :: List.map GraphEffect graphEffects
                    )
                        |> pluginNewGraph plugins

                _ ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph
                    in
                    ( { model | graph = graph }
                    , List.map GraphEffect graphEffects
                    )

        UserClickedConfirm ms ->
            update plugins uc ms model |> Tuple.mapFirst (s_dialog Nothing)

        UserClickedOption ms ->
            update plugins uc ms model |> Tuple.mapFirst (s_dialog Nothing)

        UserClickedOutsideDialog ms ->
            update plugins uc ms model |> Tuple.mapFirst (s_dialog Nothing)

        PluginMsg msgValue ->
            updatePlugins plugins uc msgValue model

        UserClosesNotification ->
            n { model | notifications = Notification.pop model.notifications }

        LocaleSelectBoxMsg subMsg ->
            let
                ( selectBox, outMsg ) =
                    model.localeSelectBox
                        |> TSelectBox.update subMsg

                newModel =
                    { model | localeSelectBox = selectBox }
            in
            case outMsg of
                TSelectBox.Selected x ->
                    switchLocale x newModel

                TSelectBox.NoSelection ->
                    n newModel

        NotificationMsg ms ->
            n { model | notifications = Notification.update ms model.notifications }

        ShowNotification nt ->
            let
                ( notifications, notificationEffects ) =
                    Notification.add nt model.notifications
            in
            ( { model
                | notifications = notifications
              }
            , List.map NotificationEffect notificationEffects
            )

        BrowserGotUncaughtError value ->
            value
                |> Json.Decode.decodeValue
                    (Json.Decode.field "message" Json.Decode.string)
                |> Result.Extra.unpack
                    (Json.Decode.errorToString
                        >> Ports.console
                        >> CmdEffect
                        >> List.singleton
                        >> pair model
                    )
                    (\message ->
                        let
                            ( notifications, eff ) =
                                Notification.add
                                    (Notification.errorDefault "uncaught-error-message"
                                        |> Notification.map (s_title (Just "An error occurred"))
                                        |> Notification.map (s_moreInfo [ message ])
                                    )
                                    model.notifications
                        in
                        ( { model | notifications = notifications }
                        , List.map NotificationEffect eff
                        )
                    )

        DebouncePluginOutMsg outMsg ->
            updateByPluginOutMsg plugins uc [ outMsg ] ( model, [] )


apiRateExceededError : Locale.Model -> Auth -> Notification
apiRateExceededError locale auth =
    let
        limited =
            case auth of
                Authorized { requestLimit } ->
                    case requestLimit of
                        Limited l ->
                            Just l

                        Unlimited ->
                            Nothing

                _ ->
                    Nothing
    in
    Notification.errorDefault
        (limited
            |> Maybe.map
                (\_ ->
                    "Info-request-limit-exceeded-with-details"
                )
            |> Maybe.withDefault "Info-request-limit-exceeded"
        )
        |> Notification.map (s_title (Just "request limit exceeded"))
        |> Notification.map
            (s_variables
                (limited
                    |> Maybe.map
                        (\{ limit, interval } ->
                            [ String.fromInt limit
                            , requestLimitIntervalToString interval
                                |> Locale.string locale
                            ]
                        )
                    |> Maybe.withDefault []
                )
            )


switchLocale : String -> Model key -> ( Model key, List Effect )
switchLocale loc model =
    let
        locale =
            Locale.switch loc model.config.locale

        newModel =
            { model
                | config =
                    model.config
                        |> s_locale locale
            }
    in
    ( newModel
    , [ Locale.getTranslationEffect loc
            |> LocaleEffect
      , saveUserSettings newModel
      ]
    )


updateByPluginOutMsg : Plugins -> Config -> List Plugin.OutMsg -> ( Model key, List Effect ) -> ( Model key, List Effect )
updateByPluginOutMsg plugins uc outMsgs ( mo, effects ) =
    let
        updateGraphByPluginOutMsg model eff subMsg =
            let
                ( graph, graphEffect ) =
                    Graph.updateByPluginOutMsg plugins [ subMsg ] model.graph

                ( pathfinder, pathfinderEffect ) =
                    Pathfinder.updateByPluginOutMsg plugins uc [ subMsg ] model.pathfinder
            in
            ( { model
                | graph = graph
                , pathfinder = pathfinder
              }
            , eff
                ++ List.map GraphEffect graphEffect
                ++ List.map PathfinderEffect pathfinderEffect
            )
    in
    outMsgs
        |> List.foldl
            (\msg ( model, eff ) ->
                case Log.truncate "outMsg" msg of
                    PluginInterface.ShowBrowser ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.UpdateAddresses _ _ ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.UpdateAddressesByRootAddress _ _ ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.UpdateAddressesByEntityPathfinder _ _ ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.UpdateAddressEntities _ _ ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.UpdateEntities _ _ ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.UpdateEntitiesByRootAddress _ _ ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.LoadAddressIntoGraph _ ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.OutMsgsPathfinder (PluginInterface.ShowPathsInPathfinder _ _) ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.OutMsgsPathfinder (PluginInterface.ShowPathsInPathfinderWithConfig _ _ _) ->
                        updateGraphByPluginOutMsg model eff msg

                    PluginInterface.GetAddressDomElement id pmsg ->
                        ( mo
                        , Id.addressIdToString id
                            |> Browser.Dom.getElement
                            |> Task.attempt (BrowserGotElementForPlugin pmsg)
                            |> CmdEffect
                            |> List.singleton
                            |> (++) eff
                        )

                    PluginInterface.PushUrl url ->
                        ( model
                        , url
                            |> NavPushUrlEffect
                            |> List.singleton
                            |> (++) eff
                        )

                    PluginInterface.GetEntitiesForAddresses addresses toMsg ->
                        let
                            -- separate loaded clusters from loading ones
                            ( ready, loading ) =
                                addresses
                                    |> List.foldl
                                        (\address ( ready_, loading_ ) ->
                                            let
                                                addr =
                                                    Dict.get (Id.initFromRecord address) model.pathfinder.network.addresses
                                                        |> Maybe.andThen (.data >> RD.toMaybe)
                                                        |> Maybe.andThen
                                                            (\a ->
                                                                Dict.get (Id.initClusterId a.currency a.entity) model.pathfinder.clusters
                                                            )
                                            in
                                            case addr of
                                                Just RD.Loading ->
                                                    ( ready_, address :: loading_ )

                                                Just (RD.Success c) ->
                                                    ( ( address, c ) :: ready_, loading_ )

                                                _ ->
                                                    ( ready_, loading_ )
                                        )
                                        ( [], [] )
                        in
                        addresses
                            |> List.filterMap
                                (\address ->
                                    Layer.getEntityForAddress address model.graph.layers
                                        |> Maybe.map (pair address)
                                )
                            |> (++) ready
                            |> (\entities ->
                                    let
                                        ( new, outMsg, cmd ) =
                                            Plugin.update plugins uc (toMsg entities) model.plugins

                                        tryAgain =
                                            if List.isEmpty loading then
                                                []

                                            else
                                                Process.sleep 100
                                                    |> Task.perform
                                                        (\_ ->
                                                            PluginInterface.GetEntitiesForAddresses
                                                                loading
                                                                toMsg
                                                                |> DebouncePluginOutMsg
                                                        )
                                                    |> CmdEffect
                                                    |> List.singleton
                                    in
                                    ( { model
                                        | plugins = new
                                      }
                                    , PluginEffect cmd
                                        :: eff
                                        ++ tryAgain
                                    )
                                        |> updateByPluginOutMsg plugins uc outMsg
                               )

                    PluginInterface.GetEntities entities toMsg ->
                        entities
                            |> List.concatMap
                                (\entity -> Layer.getEntities entity.currency entity.entity model.graph.layers)
                            |> List.map .entity
                            |> (++)
                                (Dict.values model.pathfinder.clusters
                                    |> List.filterMap RD.toMaybe
                                )
                            |> (\ents ->
                                    let
                                        ( new, outMsg, cmd ) =
                                            Plugin.update plugins uc (toMsg ents) model.plugins
                                    in
                                    ( { model
                                        | plugins = new
                                      }
                                    , PluginEffect cmd :: eff
                                    )
                                        |> updateByPluginOutMsg plugins uc outMsg
                               )

                    PluginInterface.GetSerialized toMsg ->
                        let
                            serialized =
                                Graph.encode model.graph

                            ( new, outMsg, cmd ) =
                                Plugin.update plugins uc (toMsg serialized) model.plugins
                        in
                        ( { model
                            | plugins = new
                          }
                        , PluginEffect cmd :: eff
                        )
                            |> updateByPluginOutMsg plugins uc outMsg

                    PluginInterface.OutMsgsPathfinder (PluginInterface.GetPathfinderGraphJson toMsg) ->
                        let
                            serialized =
                                Pathfinder.encode model.pathfinder

                            ( new, outMsg, cmd ) =
                                Plugin.update plugins uc (toMsg serialized) model.plugins
                        in
                        ( { model
                            | plugins = new
                          }
                        , PluginEffect cmd :: eff
                        )
                            |> updateByPluginOutMsg plugins uc outMsg

                    PluginInterface.OutMsgsPathfinder (PluginInterface.GetAddressesShown toMsg) ->
                        let
                            data =
                                model.pathfinder.network.addresses |> Dict.values

                            ( new, outMsg, cmd ) =
                                Plugin.update plugins uc (toMsg data) model.plugins
                        in
                        ( { model
                            | plugins = new
                          }
                        , PluginEffect cmd :: eff
                        )
                            |> updateByPluginOutMsg plugins uc outMsg

                    PluginInterface.Deserialize filename data ->
                        deserialize plugins uc filename data model
                            |> mapSecond ((++) eff)

                    PluginInterface.SendToPort value ->
                        ( model
                        , value
                            |> Ports.pluginsOut
                            |> CmdEffect
                            |> List.singleton
                            |> (++) eff
                        )

                    PluginInterface.ApiRequest effect ->
                        ( model
                        , (Effect.Api.map PluginMsg effect |> ApiEffect) :: eff
                        )

                    PluginInterface.ShowDialog conf ->
                        ( { model
                            | dialog =
                                Dialog.mapMsg PluginMsg conf
                                    |> Just
                          }
                        , eff
                        )

                    PluginInterface.CloseDialog ->
                        ( { model
                            | dialog = Nothing
                          }
                        , eff
                        )

                    PluginInterface.ShowNotification nt ->
                        let
                            ( notifications, notificationEffects ) =
                                Notification.add nt model.notifications
                        in
                        ( { model
                            | notifications = notifications
                          }
                        , List.map NotificationEffect notificationEffects ++ eff
                        )

                    PluginInterface.OpenTooltip s msgs ->
                        update plugins uc (OpeningTooltip s False (Tooltip.Plugin s (Tooltip.mapMsgTooltipMsg msgs PluginMsg))) mo |> Tuple.mapSecond ((++) eff)

                    PluginInterface.CloseTooltip s withDelay ->
                        update plugins uc (ClosingTooltip (Just s) withDelay) mo
                            |> Tuple.mapSecond ((++) eff)
            )
            ( mo, effects )


updateByUrl : Plugins -> Config -> Url -> Model key -> ( Model key, List Effect )
updateByUrl plugins uc url model =
    let
        routeConfig =
            model.stats
                |> RD.map (.currencies >> List.map .name)
                |> RD.withDefault []
                |> (\c ->
                        { graph = { currencies = c }
                        , pathfinder = { networks = c }
                        }
                   )
    in
    Route.parse routeConfig url
        |> Maybe.map2
            (\oldRoute route ->
                case route of
                    Route.Home ->
                        ( { model
                            | page = Home
                            , url = url
                            , tooltip = Nothing
                            , navbarSubMenu = Nothing
                            , search =
                                model.search
                                    |> s_searchType
                                        (Search.initSearchAddressAndTxs Nothing)
                          }
                        , []
                        )

                    Route.Stats ->
                        ( { model
                            | page = Stats
                            , url = url
                            , tooltip = Nothing
                            , navbarSubMenu = Nothing
                          }
                        , case oldRoute of
                            Route.Stats ->
                                []

                            _ ->
                                [ ApiEffect (Effect.Api.GetStatisticsEffect BrowserGotStatistics) ]
                        )

                    Route.Settings ->
                        ( { model
                            | page = Model.Settings
                            , url = url
                            , tooltip = Nothing
                            , navbarSubMenu = Nothing
                          }
                        , []
                        )

                    Route.Graph graphRoute ->
                        case graphRoute |> Log.log "graphRoute" of
                            Route.Graph.Plugin ( pid, value ) ->
                                let
                                    ( new, outMsg, cmd ) =
                                        Plugin.updateGraphByUrl pid plugins value model.plugins
                                in
                                ( { model
                                    | plugins = new
                                    , page = Graph
                                    , url = url
                                    , tooltip = Nothing
                                    , navbarSubMenu = Nothing
                                  }
                                , [ PluginEffect cmd ]
                                )
                                    |> updateByPluginOutMsg plugins uc outMsg

                            _ ->
                                let
                                    ( graph, graphEffect ) =
                                        Graph.updateByRoute plugins graphRoute model.graph

                                    ( nm, neff ) =
                                        Notification.add
                                            (Notification.infoDefault "pf1_deprecation_notice"
                                                |> Notification.map (s_title (Just "Deprecation notice"))
                                            )
                                            model.notifications
                                in
                                ( { model
                                    | page = Graph
                                    , graph = graph
                                    , url = url
                                    , tooltip = Nothing
                                    , navbarSubMenu = Nothing
                                    , notifications = nm
                                    , search =
                                        model.search
                                            |> s_searchType
                                                (Search.initSearchAll (model.stats |> RD.toMaybe))
                                  }
                                , (graphEffect
                                    |> List.map GraphEffect
                                  )
                                    ++ (neff |> List.map NotificationEffect)
                                )

                    Route.Pathfinder pfRoute ->
                        let
                            ( pfn, graphEffect ) =
                                Pathfinder.updateByRoute plugins uc pfRoute model.pathfinder
                        in
                        ( { model
                            | page = Pathfinder
                            , pathfinder = pfn
                            , url = url
                            , tooltip = Nothing
                            , navbarSubMenu = Nothing
                          }
                        , graphEffect
                            |> List.map PathfinderEffect
                        )

                    Route.Plugin ( pluginType, urlValue ) ->
                        let
                            ( new, outMsg, cmd ) =
                                Plugin.updateByUrl pluginType plugins uc urlValue model.plugins
                        in
                        ( { model
                            | plugins = new
                            , page = Plugin pluginType
                            , url = url
                            , tooltip = Nothing
                            , navbarSubMenu = Nothing
                          }
                        , [ PluginEffect cmd ]
                        )
                            |> updateByPluginOutMsg plugins uc outMsg
            )
            (Route.parse routeConfig model.url
                -- in case url is invalid, assume root url
                |> Maybe.Extra.orElse (Just Route.Stats)
            )
        |> Maybe.map
            (mapSecond
                ((++)
                    (if uc.size == Nothing then
                        [ GetContentsElementEffect ]

                     else
                        []
                    )
                )
            )
        |> Maybe.withDefault
            ( model
            , [ PostponeUpdateByUrlEffect url
              ]
            )


updateRequestLimit : Dict String String -> UserModel -> UserModel
updateRequestLimit headers model =
    let
        get key =
            Dict.get key headers
                |> Maybe.andThen String.toInt

        limitInterval =
            if Dict.member "x-ratelimit-limit-minute" headers then
                Just Minute

            else if Dict.member "x-ratelimit-limit-hour" headers then
                Just Hour

            else if Dict.member "x-ratelimit-limit-day" headers then
                Just Day

            else if Dict.member "x-ratelimit-limit-month" headers then
                Just Month

            else
                Nothing
    in
    { model
        | auth =
            { requestLimit =
                Maybe.map4
                    (\limit remaining reset interval ->
                        Limited
                            { limit = limit
                            , remaining = remaining
                            , reset = reset
                            , interval = interval
                            }
                    )
                    (get "ratelimit-limit")
                    (get "ratelimit-remaining")
                    (get "ratelimit-reset")
                    limitInterval
                    |> Maybe.withDefault Unlimited
            , expiration = Nothing
            , loggingOut = False
            }
                |> Authorized
    }


handleResponse : Plugins -> Config -> Result ( Http.Error, Headers, Effect.Api.Effect Msg ) ( Headers, Msg ) -> Model key -> ( Model key, List Effect )
handleResponse plugins uc result model =
    case result of
        Ok ( headers, message ) ->
            update plugins
                uc
                message
                { model
                    | user =
                        updateRequestLimit headers model.user
                            |> s_hovercard Nothing
                }

        Err ( BadStatus 401, headers, eff ) ->
            ( { model
                | user =
                    model.user
                        |> s_auth
                            (case model.user.auth of
                                Unauthorized _ effs ->
                                    Unauthorized False <| effs ++ [ eff ]

                                _ ->
                                    Unauthorized False [ eff ]
                            )
                        |> updateRequestLimit headers
              }
            , "userTool"
                |> Task.succeed
                |> Task.perform UserClickedUserIcon
                |> CmdEffect
                |> List.singleton
            )

        Err ( BadBody err, headers, _ ) ->
            let
                ( notifications, notificationEffects ) =
                    Notification.addHttpError model.notifications Nothing (Http.BadBody err)
            in
            ( { model
                | statusbar =
                    if err == Api.noExternalTransactions then
                        model.statusbar

                    else
                        Http.BadBody err
                            |> Just
                            |> Statusbar.add model.statusbar "error" []
                , notifications = notifications
                , user = updateRequestLimit headers model.user
              }
            , PortsConsoleEffect err
                :: List.map NotificationEffect notificationEffects
            )

        Err ( BadStatus 404, headers, _ ) ->
            { model
                | graph = Graph.handleNotFound model.graph
                , user = updateRequestLimit headers model.user
            }
                |> n

        Err ( BadStatus _, headers, _ ) ->
            { model | user = updateRequestLimit headers model.user }
                |> n

        Err _ ->
            n model


clearSearch : Plugins -> Model key -> ( Model key, List Effect )
clearSearch plugins model =
    let
        new =
            Plugin.clearSearch plugins model.plugins

        pf =
            model.pathfinder
    in
    { model
        | search = Search.clear model.search
        , plugins = new
        , pathfinder = { pf | search = Search.clear pf.search }
    }
        |> n


deserialize : Plugins -> Config -> String -> Value -> Model key -> ( Model key, List Effect )
deserialize plugins _ filename data model =
    Graph.deserialize data
        |> Result.map
            (\deser ->
                let
                    ( graph, graphEffects ) =
                        Graph.fromDeserialized deser model.graph

                    ( nm, neff ) =
                        Notification.add
                            (Notification.infoDefault "pf1_deprecation_notice"
                                |> Notification.map (s_title (Just "Deprecation notice"))
                            )
                            model.notifications
                in
                ( { model
                    | graph = graph
                    , page = Graph
                    , notifications = nm
                  }
                , List.map GraphEffect graphEffects
                    ++ (neff |> List.map NotificationEffect)
                )
            )
        |> Result.Extra.orElseLazy
            (\_ ->
                Pathfinder.deserialize data
                    |> Result.map
                        (\deser ->
                            let
                                ( pathfinder, pathfinderEffects ) =
                                    Pathfinder.fromDeserialized plugins deser model.pathfinder
                            in
                            ( { model
                                | pathfinder = pathfinder
                                , page = Pathfinder
                              }
                            , List.map PathfinderEffect pathfinderEffects
                            )
                        )
            )
        |> Result.Extra.unpack
            (\err ->
                let
                    httpError =
                        (case err of
                            Json.Decode.Failure message _ ->
                                message

                            _ ->
                                "could not read"
                        )
                            |> Http.BadBody

                    ( notifications, notificationEffects ) =
                        Notification.addHttpError model.notifications Nothing httpError
                in
                ( { model
                    | statusbar =
                        httpError
                            |> Just
                            |> Statusbar.add model.statusbar filename []
                    , notifications = notifications
                  }
                , (Json.Decode.errorToString err
                    |> Ports.console
                    |> CmdEffect
                  )
                    :: List.map NotificationEffect notificationEffects
                )
            )
            identity


updatePlugins : Plugins -> Config -> Plugin.Msg -> Model key -> ( Model key, List Effect )
updatePlugins plugins uc msg model =
    let
        ( new, outMsg, cmd ) =
            Plugin.update plugins uc msg model.plugins
    in
    ( { model
        | plugins = new
      }
    , [ PluginEffect cmd ]
    )
        |> updateByPluginOutMsg plugins uc outMsg


pluginNewGraph : Plugins -> ( Model key, List Effect ) -> ( Model key, List Effect )
pluginNewGraph plugins ( model, eff ) =
    let
        ( new, _, cmd ) =
            Plugin.newGraph plugins model.plugins
    in
    ( { model
        | plugins = new
      }
    , PluginEffect cmd
        :: eff
    )


updateSize : Int -> Int -> { a | size : Maybe BBox } -> { a | size : Maybe BBox }
updateSize w h model =
    { model
        | size =
            model.size
                |> Maybe.map
                    (\size ->
                        { size
                            | width = size.width + toFloat w
                            , height = size.height + toFloat h
                        }
                    )
    }


toggleShowDatesInUserLocale : Model key -> ( Model key, List Effect )
toggleShowDatesInUserLocale m =
    let
        nm =
            m |> s_config (m.config |> s_showDatesInUserLocale (not m.config.showDatesInUserLocale))

        utcLocale =
            Locale.changeTimeZone Time.utc nm.config.locale

        mwUTCtz =
            nm |> s_config (nm.config |> s_locale utcLocale)
    in
    if nm.config.showDatesInUserLocale then
        ( nm, LocaleEffect (Locale.GetTimezoneEffect LocaleMsg.BrowserSentTimezone) |> List.singleton )

    else
        ( mwUTCtz, saveUserSettings mwUTCtz |> List.singleton )


toggleSnapToGrid : Model key -> ( Model key, List Effect )
toggleSnapToGrid m =
    let
        nm =
            not m.pathfinder.config.snapToGrid
                |> flip s_snapToGrid m.pathfinder.config
                |> flip s_config m.pathfinder
                |> flip s_pathfinder m
    in
    ( nm, saveUserSettings nm |> List.singleton )


toggleShowTimeZoneOffset : Model key -> ( Model key, List Effect )
toggleShowTimeZoneOffset m =
    let
        nm =
            m |> s_config (m.config |> s_showTimeZoneOffset (not m.config.showTimeZoneOffset))
    in
    ( nm, saveUserSettings nm |> List.singleton )


toggleHighlightClusterFriends : Model key -> ( Model key, List Effect )
toggleHighlightClusterFriends m =
    let
        nm =
            not m.pathfinder.config.highlightClusterFriends
                |> flip s_highlightClusterFriends m.pathfinder.config
                |> flip s_config m.pathfinder
                |> flip s_pathfinder m
    in
    ( nm, saveUserSettings nm |> List.singleton )


togglShowTimestampOnTxEdge : Model key -> ( Model key, List Effect )
togglShowTimestampOnTxEdge m =
    let
        nm =
            m |> s_config (m.config |> s_showTimestampOnTxEdge (not m.config.showTimestampOnTxEdge))
    in
    ( nm, saveUserSettings nm |> List.singleton )


saveUserSettings : Model key -> Effect
saveUserSettings =
    SaveUserSettingsEffect << Model.userSettingsFromMainModel
