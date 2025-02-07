module Update exposing (update, updateByPluginOutMsg, updateByUrl)

import Api
import Api.Data
import Browser
import Browser.Dom
import Config.Update exposing (Config)
import DateFormat
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
import Model.Currency
import Model.Dialog as Dialog
import Model.Graph.Coords exposing (BBox)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Model.Locale as Locale
import Model.Notification as Notification
import Model.Pathfinder.Error exposing (Error(..))
import Model.Pathfinder.Tooltip as Tooltip
import Model.Search as Search
import Model.Statusbar as Statusbar
import Msg.Graph as Graph
import Msg.Locale as LocaleMsg
import Msg.Pathfinder as Pathfinder
import Msg.Search as Search
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
import Update.Search as Search
import Update.Statusbar as Statusbar
import Url exposing (Url)
import Util exposing (n)
import Util.ThemedSelectBox as TSelectBox
import Util.ThemedSelectBoxes as TSelectBoxes
import View.Locale as Locale
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
                2000.0

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
    case Log.truncate "msg" msg of
        NoOp ->
            n model

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
            in
            if hasToChange then
                ( { model | tooltip = newTooltip }
                , Cmd.map HovercardMsg cmd
                    |> CmdEffect
                    |> List.singleton
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
                ( newPluginsState, outMsg, cmdp ) =
                    PluginInterface.ClosedTooltip ctx
                        |> Plugin.updateByCoreMsg plugins uc model.plugins
            in
            (model
                |> s_plugins newPluginsState
                |> n
                |> tooltipCloseIfNotAborted
            )
                |> updateByPluginOutMsg plugins uc outMsg
                |> Tuple.mapSecond ((++) [ PluginEffect cmdp ])

        UserRequestsUrl request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Url.toString url
                        |> NavPushUrlEffect
                        |> List.singleton
                    )

                Browser.External url ->
                    ( model
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
                        model.search
                            |> s_searchType
                                (Search.initSearchAll (Just stats))
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

        BrowserGotResponseWithHeaders statusbarToken result ->
            let
                newDialog =
                    statusbarToken
                        |> Maybe.andThen
                            (\token ->
                                case result of
                                    Err ( Http.BadStatus 404, _ ) ->
                                        Statusbar.getMessage token model.statusbar
                                            |> Maybe.andThen
                                                (\( key, v ) ->
                                                    if key == Statusbar.loadingAddressKey || key == Statusbar.loadingAddressEntityKey then
                                                        Just ( key, v )

                                                    else
                                                        Nothing
                                                )
                                            |> Maybe.andThen (second >> List.Extra.getAt 0)
                                            |> Maybe.map
                                                (\address ->
                                                    UserClosesDialog
                                                        |> Dialog.addressNotFoundError address model.dialog
                                                )

                                    _ ->
                                        model.dialog
                            )
                        |> Maybe.Extra.orElse model.dialog

                isErrorDialogShown =
                    case newDialog of
                        Just (Dialog.Error _) ->
                            True

                        _ ->
                            False

                ( notifications, notificationEffects ) =
                    case ( isErrorDialogShown, result ) of
                        ( False, Err ( httpErr, _ ) ) ->
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
                                    Err ( Http.BadStatus 401, _ ) ->
                                        Nothing

                                    Err ( err, _ ) ->
                                        Just err

                                    Ok _ ->
                                        Nothing
                                )
                                model.statusbar

                        Nothing ->
                            case result of
                                Err ( Http.BadStatus 429, _ ) ->
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
                    n { model | dialog = Nothing, tooltip = Nothing }

                _ ->
                    n model

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
            ( newModel, [ SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) ] )

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
                    , selectBoxes = TSelectBoxes.closeAll model.selectBoxes
                    , plugins = new
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
                search =
                    Search.setQuery str model.search
                        |> s_visible True
            in
            update plugins uc (Search.UserFocusSearch |> SearchMsg) model
                |> mapFirst (s_search search)
                |> mapSecond
                    ((++) (Search.maybeTriggerSearch search |> List.map SearchEffect))

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

        BrowserGotLoggedOut _ ->
            ( model
            , [ NavLoadEffect "/" ]
            )

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
                            [ SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) ]

                        _ ->
                            []
            in
            ( newModel
            , List.map LocaleEffect localeEffects ++ eff
            )

        SettingsMsg (UserChangedPreferredCurrency currency) ->
            let
                locale =
                    Locale.changeCurrency currency model.config.locale

                newModel =
                    { model
                        | config =
                            model.config
                                |> s_locale locale
                                |> s_preferredFiatCurrency currency
                    }
            in
            ( newModel, [ SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) ] )

        SettingsMsg UserToggledValueDisplay ->
            let
                showInFiat =
                    not model.config.showValuesInFiat

                locale =
                    if showInFiat then
                        Locale.changeCurrency model.config.preferredFiatCurrency model.config.locale

                    else
                        Locale.changeCurrency "coin" model.config.locale

                newModel =
                    { model
                        | config =
                            model.config
                                |> s_locale locale
                                |> s_showValuesInFiat showInFiat
                    }
            in
            ( newModel, [ SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) ] )

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
                                                    { message = Locale.string model.config.locale "Please choose a crypto ledger"
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
                            Graph.loadAddressPath plugins
                                { currency = currency
                                , addresses =
                                    Search.query model.search
                                        |> Search.getMulti
                                }
                                model.graph

                        ( search, searchEffects ) =
                            Search.update m model.search
                    in
                    clearSearch plugins { model | graph = graph, search = search, dialog = Nothing }
                        |> mapSecond ((++) (List.map GraphEffect graphEffects))
                        |> mapSecond ((++) (List.map SearchEffect searchEffects))
                        |> mapSecond
                            ((++)
                                [ Route.Graph.Root
                                    |> Route.graphRoute
                                    |> Route.toUrl
                                    |> NavPushUrlEffect
                                ]
                            )

                _ ->
                    let
                        ( search, searchEffects ) =
                            Search.update m model.search
                    in
                    ( { model | search = search }
                    , List.map SearchEffect searchEffects
                    )

        PathfinderMsg Pathfinder.UserClickedRestart ->
            if model.pathfinder.isDirty then
                n
                    { model
                        | dialog =
                            { message = Locale.string model.config.locale "You will not be able to recover it."
                            , confirmText = Just "Yes, delete all"
                            , cancelText = Just "Cancel"
                            , title = "Clear dashboard?"
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
                    model.stats |> RD.map (\x -> Init.Pathfinder.init (Model.userSettingsFromMainModel model) (Just x)) |> RD.withDefault ( model.pathfinder, Cmd.none )

                ( newPluginsState, outMsg, cmdp ) =
                    PluginInterface.Reset
                        |> Plugin.updateByCoreMsg plugins uc model.plugins
            in
            ( { model | pathfinder = m, plugins = newPluginsState }, [ CmdEffect (cmd |> Cmd.map PathfinderMsg) ] )
                |> updateByPluginOutMsg plugins uc outMsg
                |> Tuple.mapSecond ((++) [ PluginEffect cmdp ])

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

                Pathfinder.UserClickedToggleValueDisplay ->
                    update plugins uc (UserToggledValueDisplay |> SettingsMsg) model

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

                        Pathfinder.BrowserGotAddressData id data ->
                            let
                                ( new, outMsg, cmd ) =
                                    id
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

                        Pathfinder.PluginMsg ms ->
                            updatePlugins plugins uc ms model

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
            if newModel.pathfinder.network == pathfinderOld.network then
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
                            Plugin.entitiesAdded plugins model.plugins ids

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
                        locale =
                            Locale.changeCurrency currency model.config.locale

                        newModel =
                            { model
                                | config =
                                    model.config
                                        |> s_locale locale
                                        |> s_showValuesInFiat (locale.currency /= Model.Currency.Coin)
                            }
                    in
                    ( newModel, [ SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) ] )

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
                    ( newModel, [ SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) ] )

                Graph.UserClickedShowEntityShadowLinks ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) :: List.map GraphEffect graphEffects )

                Graph.UserClickedShowAddressShadowLinks ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) :: List.map GraphEffect graphEffects )

                Graph.UserClickedToggleShowZeroTransactions ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) :: List.map GraphEffect graphEffects )

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
                    ( newModel, SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) :: List.map GraphEffect graphEffects )

                Graph.UserChangesTxLabelType _ ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph

                        newModel =
                            { model | graph = graph }
                    in
                    ( newModel, SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel) :: List.map GraphEffect graphEffects )

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
                            { model
                                | graph = Graph.importTagPack uc yaml model.graph
                            }
                                |> n

                Graph.PortDeserializedGS ( filename, data ) ->
                    pluginNewGraph plugins ( model, [] )
                        |> (\( mdl, eff ) ->
                                deserialize plugins filename data mdl
                                    |> mapSecond ((++) eff)
                           )

                Graph.UserClickedNew ->
                    if model.dirty then
                        { model
                            | dialog =
                                { message = Locale.string model.config.locale "Do you want to start from scratch?"
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

        SelectBoxMsg sb subMsg ->
            let
                ( selectBoxes, outMsg ) =
                    model.selectBoxes
                        |> TSelectBoxes.update sb subMsg

                newModel =
                    { model | selectBoxes = selectBoxes }
            in
            case ( sb, outMsg ) of
                ( TSelectBoxes.SupportedLanguages, Just (TSelectBox.Selected x) ) ->
                    switchLocale x newModel

                _ ->
                    n newModel

        NotificationMsg ms ->
            n { model | notifications = Notification.update ms model.notifications }


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
      , SaveUserSettingsEffect (Model.userSettingsFromMainModel newModel)
      ]
    )


updateByPluginOutMsg : Plugins -> Config -> List Plugin.OutMsg -> ( Model key, List Effect ) -> ( Model key, List Effect )
updateByPluginOutMsg plugins uc outMsgs ( mo, effects ) =
    let
        updateGraphByPluginOutMsg model eff =
            let
                ( graph, graphEffect ) =
                    Graph.updateByPluginOutMsg plugins outMsgs model.graph

                ( pathfinder, pathfinderEffect ) =
                    Pathfinder.updateByPluginOutMsg plugins outMsgs model.pathfinder
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
                case Log.log "outMsg" msg of
                    PluginInterface.ShowBrowser ->
                        updateGraphByPluginOutMsg model eff

                    PluginInterface.UpdateAddresses _ _ ->
                        updateGraphByPluginOutMsg model eff

                    PluginInterface.UpdateAddressEntities _ _ ->
                        updateGraphByPluginOutMsg model eff

                    PluginInterface.UpdateEntities _ _ ->
                        updateGraphByPluginOutMsg model eff

                    PluginInterface.UpdateEntitiesByRootAddress _ _ ->
                        updateGraphByPluginOutMsg model eff

                    PluginInterface.LoadAddressIntoGraph _ ->
                        updateGraphByPluginOutMsg model eff

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
                        addresses
                            |> List.filterMap
                                (\address ->
                                    Layer.getEntityForAddress address model.graph.layers
                                        |> Maybe.map (pair address)
                                )
                            |> (\entities ->
                                    let
                                        ( new, outMsg, cmd ) =
                                            Plugin.update plugins uc (toMsg entities) model.plugins
                                    in
                                    ( { model
                                        | plugins = new
                                      }
                                    , PluginEffect cmd :: eff
                                    )
                                        |> updateByPluginOutMsg plugins uc outMsg
                               )

                    PluginInterface.GetEntities entities toMsg ->
                        entities
                            |> List.concatMap
                                (\entity -> Layer.getEntities entity.currency entity.entity model.graph.layers)
                            |> List.map .entity
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

                    PluginInterface.Deserialize filename data ->
                        deserialize plugins filename data model
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
                        , List.map NotificationEffect notificationEffects
                        )

                    PluginInterface.OpenTooltip s ->
                        update plugins uc (OpenTooltip s (Tooltip.Plugin s)) mo |> Tuple.mapSecond ((++) effects)

                    PluginInterface.CloseTooltip s withDelay ->
                        update plugins uc (ClosingTooltip (Just s) withDelay) mo
                            |> Tuple.mapSecond ((++) effects)
            )
            ( mo, effects )


updateByUrl : Plugins -> Config -> Url -> Model key -> ( Model key, List Effect )
updateByUrl plugins uc url model =
    let
        modelReady =
            not (Locale.isEmpty model.config.locale)
                && RD.isSuccess model.stats
    in
    if not modelReady then
        ( model
        , [ PostponeUpdateByUrlEffect url
          ]
        )

    else
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
                              }
                            , []
                            )

                        Route.Stats ->
                            ( { model
                                | page = Stats
                                , url = url
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
                                      }
                                    , [ PluginEffect cmd ]
                                    )
                                        |> updateByPluginOutMsg plugins uc outMsg

                                _ ->
                                    let
                                        ( graph, graphEffect ) =
                                            Graph.updateByRoute plugins graphRoute model.graph
                                    in
                                    ( { model
                                        | page = Graph
                                        , graph = graph
                                        , url = url
                                      }
                                    , graphEffect
                                        |> List.map GraphEffect
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
            |> Maybe.withDefault (n model)


updateRequestLimit : Dict String String -> UserModel -> UserModel
updateRequestLimit headers model =
    let
        get key =
            Dict.get key headers
                |> Maybe.andThen String.toInt
    in
    { model
        | auth =
            { requestLimit =
                Maybe.map3
                    (\limit remaining reset ->
                        Limited { limit = limit, remaining = remaining, reset = reset }
                    )
                    (get "ratelimit-limit")
                    (get "ratelimit-remaining")
                    (get "ratelimit-reset")
                    |> Maybe.withDefault Unlimited
            , expiration = Nothing
            , loggingOut = False
            }
                |> Authorized
    }


handleResponse : Plugins -> Config -> Result ( Http.Error, Effect.Api.Effect Msg ) ( Dict String String, Msg ) -> Model key -> ( Model key, List Effect )
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

        Err ( BadStatus 401, eff ) ->
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
              }
            , "userTool"
                |> Task.succeed
                |> Task.perform UserClickedUserIcon
                |> CmdEffect
                |> List.singleton
            )

        Err ( BadBody err, _ ) ->
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
              }
            , PortsConsoleEffect err
                :: List.map NotificationEffect notificationEffects
            )

        Err ( BadStatus 404, _ ) ->
            { model
                | graph = Graph.handleNotFound model.graph
            }
                |> n

        Err _ ->
            n model


makeTimestampFilename : Locale.Model -> Time.Posix -> String
makeTimestampFilename locale t =
    Time.posixToMillis t
        // 1000
        |> Locale.timestampWithFormat
            [ DateFormat.yearNumber
            , DateFormat.text "-"
            , DateFormat.monthFixed
            , DateFormat.text "-"
            , DateFormat.dayOfMonthFixed
            , DateFormat.text " "
            , DateFormat.hourMilitaryFixed
            , DateFormat.text "-"
            , DateFormat.minuteFixed
            , DateFormat.text "-"
            , DateFormat.secondFixed
            ]
            locale


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


deserialize : Plugins -> String -> Value -> Model key -> ( Model key, List Effect )
deserialize plugins filename data model =
    Graph.deserialize data
        |> Result.map
            (\deser ->
                let
                    ( graph, graphEffects ) =
                        Graph.fromDeserialized deser model.graph
                in
                ( { model
                    | graph = graph
                    , page = Graph
                  }
                , List.map GraphEffect graphEffects
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
        ( mwUTCtz, SaveUserSettingsEffect (Model.userSettingsFromMainModel mwUTCtz) |> List.singleton )


toggleSnapToGrid : Model key -> ( Model key, List Effect )
toggleSnapToGrid m =
    let
        nm =
            m |> s_config (m.config |> s_snapToGrid (not m.config.snapToGrid))
    in
    ( nm, SaveUserSettingsEffect (Model.userSettingsFromMainModel nm) |> List.singleton )


toggleShowTimeZoneOffset : Model key -> ( Model key, List Effect )
toggleShowTimeZoneOffset m =
    let
        nm =
            m |> s_config (m.config |> s_showTimeZoneOffset (not m.config.showTimeZoneOffset))
    in
    ( nm, SaveUserSettingsEffect (Model.userSettingsFromMainModel nm) |> List.singleton )


toggleHighlightClusterFriends : Model key -> ( Model key, List Effect )
toggleHighlightClusterFriends m =
    let
        nm =
            m |> s_config (m.config |> s_highlightClusterFriends (not m.config.highlightClusterFriends))
    in
    ( nm, SaveUserSettingsEffect (Model.userSettingsFromMainModel nm) |> List.singleton )


togglShowTimestampOnTxEdge : Model key -> ( Model key, List Effect )
togglShowTimestampOnTxEdge m =
    let
        nm =
            m |> s_config (m.config |> s_showTimestampOnTxEdge (not m.config.showTimestampOnTxEdge))
    in
    ( nm, SaveUserSettingsEffect (Model.userSettingsFromMainModel nm) |> List.singleton )
