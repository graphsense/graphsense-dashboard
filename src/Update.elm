module Update exposing (update, updateByPluginOutMsg, updateByUrl)

--import Plugin.Update.Graph

import Api
import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Config.Update exposing (Config)
import DateFormat
import Dict exposing (Dict)
import Effect exposing (n)
import Effect.Api
import Effect.Graph as Graph
import Effect.Locale as Locale
import File.Download
import Http exposing (Error(..))
import Json.Decode
import Json.Encode exposing (Value)
import Lense
import List.Extra
import Log
import Maybe.Extra
import Model exposing (..)
import Model.Dialog as Dialog
import Model.Graph.Browser as Browser
import Model.Graph.Coords exposing (BBox)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Model.Locale as Locale
import Model.Search as Search
import Model.Statusbar as Statusbar
import Msg.Graph as Graph
import Msg.Search as Search
import Plugin.Model as Plugin
import Plugin.Msg as Plugin
import Plugin.Update as Plugin exposing (Plugins)
import PluginInterface.Msg as PluginInterface
import PluginInterface.Update as PluginInterface
import Ports
import Process
import RecordSetter exposing (..)
import RemoteData as RD
import Route
import Route.Graph
import Set
import Sha256
import Task
import Time
import Tuple exposing (..)
import Update.Dialog as Dialog
import Update.Graph as Graph
import Update.Graph.Adding as Adding
import Update.Graph.Browser as Browser
import Update.Graph.Layer as Layer
import Update.Graph.Search
import Update.Locale as Locale
import Update.Search as Search
import Update.Statusbar as Statusbar
import Url exposing (Url)
import View.Locale as Locale
import Yaml.Decode


update : Plugins -> Config -> Msg -> Model key -> ( Model key, List Effect )
update plugins uc msg model =
    case Log.truncate "msg" msg of
        NoOp ->
            n model

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

        BrowserGotStatistics result ->
            case result of
                Ok stats ->
                    updateByUrl plugins
                        uc
                        model.url
                        { model
                            | stats = RD.Success stats
                            , statusbar = Statusbar.updateLastBlocks stats model.statusbar
                        }

                Err error ->
                    n { model | stats = RD.Failure error }

        BrowserGotEntityTaxonomy concepts ->
            { model
                | graph =
                    Graph.setEntityConcepts concepts model.graph
            }
                |> n

        BrowserGotAbuseTaxonomy concepts ->
            { model
                | graph =
                    Graph.setAbuseConcepts concepts model.graph
            }
                |> n

        BrowserGotSupportedTokens configs ->
            let
                locale =
                    Locale.supportedTokens configs model.locale
            in
            { model
                | supportedTokens = Just configs
                , locale = locale
                , config =
                    model.config
                        |> s_locale locale
            }
                |> n

        BrowserGotResponseWithHeaders statusbarToken result ->
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
                , dialog =
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
                                                        |> Dialog.addressNotFound address model.dialog
                                                )

                                    _ ->
                                        model.dialog
                            )
                        |> Maybe.Extra.orElse model.dialog
            }
                |> handleResponse plugins
                    uc
                    result

        UserClosesDialog ->
            case model.dialog of
                Just (Dialog.Error _) ->
                    n { model | dialog = Nothing }

                _ ->
                    n model

        UserHoversUserIcon id ->
            ( model
            , GetElementEffect
                { id = id
                , msg = BrowserGotElement
                }
                |> List.singleton
            )

        UserLeftUserHovercard ->
            { model
                | user =
                    case model.user.auth of
                        Unauthorized _ _ ->
                            model.user

                        _ ->
                            model.user |> s_hovercardElement Nothing
            }
                |> n

        UserSwitchesLocale loc ->
            let
                locale =
                    Locale.switch loc model.locale
            in
            ( { model
                | locale = locale
                , config =
                    model.config
                        |> s_locale locale
              }
            , Locale.getTranslationEffect loc
                |> LocaleEffect
                |> List.singleton
            )

        UserClickedLightmode ->
            { model
                | config =
                    model.config
                        |> s_lightmode (not model.config.lightmode)
            }
                |> n

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
                            |> s_hovercardElement
                                (if List.isEmpty effs then
                                    Nothing

                                 else
                                    model.user.hovercardElement
                                )
                    , plugins = new
                  }
                , PluginEffect cmd
                    :: effs
                )
                    |> updateByPluginOutMsg plugins outMsg

        BrowserGotElement result ->
            { model
                | user =
                    model.user
                        |> s_hovercardElement (Result.toMaybe result)
            }
                |> n

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
            clearSearch plugins
                { model
                    | user =
                        model.user
                            |> s_hovercardElement Nothing
                }

        TimeUpdateReset time ->
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
                |> updateByPluginOutMsg plugins outMsg

        BrowserGotLoggedOut result ->
            { model
                | user =
                    result
                        |> Result.map
                            (\_ ->
                                model.user
                                    |> s_auth (Unauthorized False [])
                                    |> s_apiKey ""
                            )
                        |> Result.withDefault
                            (model.user
                                |> s_auth
                                    (case model.user.auth of
                                        Authorized auth ->
                                            { auth | loggingOut = False }
                                                |> Authorized

                                        _ ->
                                            model.user.auth
                                    )
                            )
            }
                |> n

        BrowserGotElementForPlugin pmsg element ->
            updatePlugins plugins (pmsg element) model

        LocaleMsg m ->
            let
                ( locale, localeEffects ) =
                    Locale.update m model.locale
            in
            ( { model
                | locale = locale
                , config =
                    model.config
                        |> s_locale locale
              }
            , List.map LocaleEffect localeEffects
            )

        SearchMsg m ->
            case m of
                Search.PluginMsg ms ->
                    updatePlugins plugins ms model

                Search.UserHitsEnter ->
                    let
                        multi =
                            Search.getMulti model.search
                    in
                    if model.search.found /= Nothing && List.length multi == 1 then
                        Search.getFirstResultUrl model.search
                            |> Maybe.map
                                (NavPushUrlEffect >> List.singleton)
                            |> Maybe.withDefault []
                            |> pair { model | search = Search.clear model.search }

                    else
                        model.stats
                            |> RD.map
                                (\stats ->
                                    { model
                                        | dialog =
                                            { message = Locale.string model.locale "Please choose a crypto ledger"
                                            , options =
                                                stats.currencies
                                                    |> List.map .name
                                                    |> List.map
                                                        (\name ->
                                                            ( String.toUpper name
                                                            , Search.UserPicksCurrency name |> SearchMsg
                                                            )
                                                        )
                                            }
                                                |> Dialog.options
                                                |> Just
                                    }
                                )
                            |> RD.withDefault model
                            |> n

                Search.UserPicksCurrency currency ->
                    let
                        ( graph, graphEffects ) =
                            Graph.loadPath plugins
                                { currency = currency
                                , addresses = Search.getMulti model.search
                                }
                                model.graph
                    in
                    clearSearch plugins { model | graph = graph, dialog = Nothing }
                        |> mapSecond ((++) (List.map GraphEffect graphEffects))

                Search.BouncedBlur ->
                    clearSearch plugins model

                _ ->
                    let
                        ( search, searchEffects ) =
                            Search.update m model.search
                    in
                    ( { model | search = search }
                    , List.map SearchEffect searchEffects
                    )

        GraphMsg m ->
            case m of
                Graph.PluginMsg ms ->
                    updatePlugins plugins ms model

                Graph.InternalGraphAddedAddresses ids ->
                    let
                        ( new, outMsg, cmd ) =
                            Plugin.addressesAdded plugins model.plugins ids

                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph
                    in
                    ( { model
                        | plugins = new
                        , graph = graph
                      }
                    , PluginEffect cmd
                        :: List.map GraphEffect graphEffects
                    )
                        |> updateByPluginOutMsg plugins outMsg

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
                      }
                    , PluginEffect cmd
                        :: List.map GraphEffect graphEffects
                    )
                        |> updateByPluginOutMsg plugins outMsg

                Graph.UserChangesCurrency currency ->
                    let
                        locale =
                            Locale.changeCurrency currency model.locale
                    in
                    { model
                        | locale = locale
                        , config =
                            model.config
                                |> s_locale locale
                    }
                        |> n

                Graph.UserChangesValueDetail detail ->
                    let
                        locale =
                            Locale.changeValueDetail detail model.locale
                    in
                    { model
                        | locale = locale
                        , config =
                            model.config
                                |> s_locale locale
                    }
                        |> n

                Graph.UserClickedExportGS time ->
                    ( model
                    , (case time of
                        Nothing ->
                            Time.now
                                |> Task.perform (Just >> Graph.UserClickedExportGS)

                        Just t ->
                            Graph.serialize model.graph
                                |> pair
                                    (makeTimestampFilename model.locale t
                                        |> (\tt -> tt ++ ".gs")
                                    )
                                |> Ports.serialize
                      )
                        |> Graph.CmdEffect
                        |> GraphEffect
                        |> List.singleton
                    )

                Graph.UserClickedExportGraphics time ->
                    ( model
                    , (case time of
                        Nothing ->
                            Time.now
                                |> Task.perform (Just >> Graph.UserClickedExportGraphics)

                        Just t ->
                            makeTimestampFilename model.locale t
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
                                    makeTimestampFilename model.locale t
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
                            { model
                                | statusbar =
                                    Yaml.Decode.errorToString err
                                        |> Http.BadBody
                                        |> Just
                                        |> Statusbar.add model.statusbar filename []
                            }
                                |> n

                        Ok yaml ->
                            { model
                                | graph = Graph.importTagPack uc yaml model.graph
                            }
                                |> n

                Graph.PortDeserializedGS ( filename, data ) ->
                    pluginNewGraph plugins ( model, [] )
                        |> (\( mdl, eff ) ->
                                deserialize filename data mdl
                                    |> mapSecond ((++) eff)
                           )

                Graph.UserClickedNew ->
                    { model
                        | dialog =
                            { message = Locale.string model.locale "Do you want to start from scratch?"
                            , onYes = GraphMsg Graph.UserClickedNewYes
                            , onNo = NoOp
                            }
                                |> Dialog.confirm
                                |> Just
                    }
                        |> n

                Graph.UserClickedNewYes ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph
                    in
                    ( { model
                        | graph = graph
                      }
                    , (Route.Graph.Root
                        |> Route.graphRoute
                        |> Route.toUrl
                        |> NavPushUrlEffect
                      )
                        :: List.map GraphEffect graphEffects
                    )
                        |> pluginNewGraph plugins

                Graph.UserClickedGraph _ ->
                    if model.search.visible then
                        n model

                    else
                        let
                            ( graph, graphEffects ) =
                                Graph.update plugins uc m model.graph
                        in
                        ( { model | graph = graph }
                        , List.map GraphEffect graphEffects
                        )

                _ ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph
                    in
                    ( { model | graph = graph }
                    , List.map GraphEffect graphEffects
                    )

        UserClickedConfirm ms ->
            update plugins uc ms { model | dialog = Nothing }

        UserClickedOption ms ->
            update plugins uc ms { model | dialog = Nothing }

        PluginMsg msgValue ->
            updatePlugins plugins msgValue model


updateByPluginOutMsg : Plugins -> List Plugin.OutMsg -> ( Model key, List Effect ) -> ( Model key, List Effect )
updateByPluginOutMsg plugins outMsgs ( mo, effects ) =
    let
        updateGraphByPluginOutMsg model eff =
            let
                ( graph, graphEffect ) =
                    Graph.updateByPluginOutMsg plugins outMsgs model.graph
            in
            ( { model
                | graph = graph
              }
            , eff ++ List.map GraphEffect graphEffect
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
                                            Plugin.update plugins (toMsg entities) model.plugins
                                    in
                                    ( { model
                                        | plugins = new
                                      }
                                    , PluginEffect cmd :: eff
                                    )
                                        |> updateByPluginOutMsg plugins outMsg
                               )

                    PluginInterface.GetEntities entities toMsg ->
                        entities
                            |> List.map
                                (\entity -> Layer.getEntities entity.currency entity.entity model.graph.layers)
                            |> List.concat
                            |> List.map .entity
                            |> (\ents ->
                                    let
                                        ( new, outMsg, cmd ) =
                                            Plugin.update plugins (toMsg ents) model.plugins
                                    in
                                    ( { model
                                        | plugins = new
                                      }
                                    , PluginEffect cmd :: eff
                                    )
                                        |> updateByPluginOutMsg plugins outMsg
                               )

                    PluginInterface.GetSerialized toMsg ->
                        let
                            serialized =
                                Graph.serialize model.graph

                            ( new, outMsg, cmd ) =
                                Plugin.update plugins (toMsg serialized) model.plugins
                        in
                        ( { model
                            | plugins = new
                          }
                        , PluginEffect cmd :: eff
                        )
                            |> updateByPluginOutMsg plugins outMsg

                    PluginInterface.Deserialize filename data ->
                        deserialize filename data model
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

                    PluginInterface.ShowConfirmDialog conf ->
                        ( { model
                            | dialog =
                                Dialog.confirm
                                    { message = conf.message
                                    , onYes = PluginMsg conf.onYes
                                    , onNo = PluginMsg conf.onNo
                                    }
                                    |> Just
                          }
                        , eff
                        )
            )
            ( mo, effects )


updateByUrl : Plugins -> Config -> Url -> Model key -> ( Model key, List Effect )
updateByUrl plugins uc url model =
    let
        routeConfig =
            model.stats
                |> RD.map (.currencies >> List.map .name)
                |> RD.withDefault []
                |> (\c -> { currencies = c })
                |> (\g -> { graph = g })
    in
    Route.parse routeConfig url
        |> Maybe.map2
            (\oldRoute route ->
                case Log.log "route" route of
                    Route.Stats ->
                        ( { model
                            | page = Stats
                            , url = url
                          }
                        , case oldRoute of
                            Route.Stats ->
                                []

                            _ ->
                                [ GetStatisticsEffect ]
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
                                    |> updateByPluginOutMsg plugins outMsg

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

                    Route.Plugin ( pluginType, urlValue ) ->
                        let
                            ( new, outMsg, cmd ) =
                                Plugin.updateByUrl pluginType plugins urlValue model.plugins
                        in
                        ( { model
                            | plugins = new
                            , page = Plugin pluginType
                            , url = url
                          }
                        , [ PluginEffect cmd ]
                        )
                            |> updateByPluginOutMsg plugins outMsg
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
                            |> s_hovercardElement Nothing
                }

        Err ( BadStatus 401, eff ) ->
            ( { model
                | user =
                    model.user
                        |> s_auth
                            (case model.user.auth of
                                Unauthorized loading effs ->
                                    Unauthorized False <| effs ++ [ eff ]

                                _ ->
                                    Unauthorized False [ eff ]
                            )
              }
            , "userTool"
                |> Task.succeed
                |> Task.perform UserHoversUserIcon
                |> CmdEffect
                |> List.singleton
            )

        Err ( BadBody err, _ ) ->
            ( { model
                | statusbar =
                    if err == Api.noExternalTransactions then
                        model.statusbar

                    else
                        Http.BadBody err
                            |> Just
                            |> Statusbar.add model.statusbar "error" []
              }
            , PortsConsoleEffect err
                |> List.singleton
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
    in
    { model
        | search = Search.clear model.search
        , plugins = new
    }
        |> n


deserialize : String -> Value -> Model key -> ( Model key, List Effect )
deserialize filename data model =
    case Graph.deserialize data of
        Err err ->
            ( { model
                | statusbar =
                    (case err of
                        Json.Decode.Failure message _ ->
                            message

                        _ ->
                            "could not read"
                    )
                        |> Http.BadBody
                        |> Just
                        |> Statusbar.add model.statusbar filename []
              }
            , Json.Decode.errorToString err
                |> Ports.console
                |> CmdEffect
                |> List.singleton
            )

        Ok deser ->
            let
                ( graph, graphEffects ) =
                    Graph.fromDeserialized deser model.graph
            in
            ( { model | graph = graph }
            , List.map GraphEffect graphEffects
            )


updatePlugins : Plugins -> Plugin.Msg -> Model key -> ( Model key, List Effect )
updatePlugins plugins msg model =
    let
        ( new, outMsg, cmd ) =
            Plugin.update plugins msg model.plugins
    in
    ( { model
        | plugins = new
      }
    , [ PluginEffect cmd ]
    )
        |> updateByPluginOutMsg plugins outMsg


pluginNewGraph : Plugins -> ( Model key, List Effect ) -> ( Model key, List Effect )
pluginNewGraph plugins ( model, eff ) =
    let
        ( new, outMsg, cmd ) =
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
