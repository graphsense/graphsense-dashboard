module Update exposing (update, updateByUrl)

import Browser
import Browser.Navigation as Nav
import Config.Update exposing (Config)
import Dict exposing (Dict)
import Effect exposing (n)
import Effect.Graph as Graph
import Effect.Locale as Locale
import Http exposing (Error(..))
import Json.Encode exposing (Value)
import Log
import Model exposing (..)
import Model.Graph.Browser as Browser
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Msg.Graph as Graph
import Msg.Search as Search
import Page
import Plugin as Plugin exposing (Plugins)
import Plugin.Model as Plugin
import Plugin.Update.Graph
import RecordSetter exposing (..)
import RemoteData as RD
import Route
import Route.Graph
import Task
import Tuple exposing (..)
import Update.Graph as Graph
import Update.Graph.Adding as Adding
import Update.Graph.Browser as Browser
import Update.Graph.Layer as Layer
import Update.Locale as Locale
import Update.Search as Search
import Url exposing (Url)


update : Plugins -> Config -> Msg -> Model key -> ( Model key, List Effect )
update plugins uc msg model =
    case msg of
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
                    updateByUrl plugins uc model.url { model | stats = RD.Success stats }

                Err error ->
                    n { model | stats = RD.Failure error }

        BrowserGotEntityTaxonomy concepts ->
            { model
                | entityConcepts = concepts
            }
                |> n

        BrowserGotAbuseTaxonomy concepts ->
            { model
                | abuseConcepts = concepts
            }
                |> n

        BrowserGotResponseWithHeaders result ->
            case result of
                Ok ( headers, message ) ->
                    update plugins
                        uc
                        message
                        { model
                            | user =
                                updateRequestLimit headers model.user
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
                    ( model
                    , PortsConsoleEffect err
                        |> List.singleton
                    )

                Err _ ->
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

        UserSwitchesLocale locale ->
            ( { model | locale = Locale.switch locale model.locale }
            , Locale.getTranslationEffect locale
                |> LocaleEffect
                |> List.singleton
            )

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
                                effects

                            _ ->
                                []
                in
                ( { model
                    | user =
                        model.user
                            |> s_auth
                                (Unauthorized True [])
                  }
                , effs
                )

        BrowserGotElement result ->
            { model
                | user =
                    model.user
                        |> s_hovercardElement (Result.toMaybe result)
            }
                |> n

        BrowserChangedWindowSize w h ->
            { model
                | graph = Graph.updateSize (w - model.width) (h - model.height) model.graph
                , width = w
                , height = h
            }
                |> n

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
                Search.PluginMsg pid ms ->
                    let
                        ( new, outMsg, cmd ) =
                            Plugin.update pid plugins model.plugins ms .model
                    in
                    { model
                        | plugins = new
                    }
                        |> updateByPluginOutMsg plugins pid outMsg
                        |> mapSecond ((++) (List.map PluginEffect cmd))

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
                Graph.PluginMsg pid ms ->
                    let
                        ( new, outMsg, cmd ) =
                            Plugin.update pid plugins model.plugins ms .model
                    in
                    { model
                        | plugins = new
                    }
                        |> updateByPluginOutMsg plugins pid outMsg
                        |> mapSecond ((++) (List.map PluginEffect cmd))

                Graph.InternalGraphAddedAddresses ids ->
                    let
                        ( new, outMsg, cmd ) =
                            Plugin.Update.Graph.addressesAdded plugins model.plugins ids
                    in
                    ( { model
                        | plugins = new
                      }
                    , List.map PluginEffect cmd
                    )

                _ ->
                    let
                        ( graph, graphEffects ) =
                            Graph.update plugins uc m model.graph
                    in
                    ( { model | graph = graph }
                    , List.map GraphEffect graphEffects
                    )

        PluginMsg pid msgValue ->
            let
                ( new, outMsg, cmd ) =
                    Plugin.update pid plugins model.plugins msgValue .model
            in
            { model
                | plugins = new
            }
                |> updateByPluginOutMsg plugins pid outMsg
                |> mapSecond ((++) (List.map PluginEffect cmd))


updateByPluginOutMsg : Plugins -> String -> List (Plugin.OutMsg Value) -> Model key -> ( Model key, List Effect )
updateByPluginOutMsg plugins pid outMsgs model =
    outMsgs
        |> List.foldl
            (\msg ( mo, eff ) ->
                case Log.log "outMsg" msg of
                    Plugin.ShowBrowser ->
                        ( { model
                            | graph =
                                mo.graph
                                    |> (\graph ->
                                            { graph
                                                | browser = Browser.showPlugin pid graph.browser
                                            }
                                       )
                          }
                        , eff
                        )

                    Plugin.UpdateAddresses id msgValue ->
                        let
                            layers =
                                Layer.updateAddresses id (Plugin.updateAddress pid plugins msgValue) mo.graph.layers
                        in
                        ( { model
                            | graph =
                                mo.graph
                                    |> (\graph ->
                                            { graph
                                                | layers = layers
                                                , browser =
                                                    case graph.browser.type_ of
                                                        Browser.Address (Browser.Loaded ad) table ->
                                                            if ad.address.currency == id.currency && ad.address.address == id.address then
                                                                graph.browser
                                                                    |> s_type_
                                                                        (Layer.getAddress ad.id layers
                                                                            |> Maybe.map (\a -> Browser.Address (Browser.Loaded a) table)
                                                                            |> Maybe.withDefault graph.browser.type_
                                                                        )

                                                            else
                                                                graph.browser

                                                        _ ->
                                                            graph.browser
                                            }
                                       )
                          }
                        , eff
                        )

                    Plugin.PushGraphUrl url ->
                        ( mo
                        , url
                            |> pair pid
                            |> Route.Graph.pluginRoute
                            |> Route.graphRoute
                            |> Route.toUrl
                            |> NavPushUrlEffect
                            |> List.singleton
                            |> (++) eff
                        )
            )
            ( model, [] )


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
    Route.parse plugins routeConfig url
        |> Maybe.map2
            (\oldRoute route ->
                case Log.log "route" route of
                    Route.Graph graphRoute ->
                        case graphRoute of
                            Route.Graph.Plugin ( pid, value ) ->
                                let
                                    ( new, outMsg, cmd ) =
                                        Plugin.updateByRoute pid plugins model.plugins value
                                in
                                { model
                                    | plugins = new
                                    , page = Page.Graph
                                    , url = url
                                }
                                    |> updateByPluginOutMsg plugins pid outMsg
                                    |> mapSecond ((++) (List.map PluginEffect cmd))

                            _ ->
                                let
                                    ( graph, graphEffect ) =
                                        Graph.updateByRoute plugins graphRoute model.graph
                                in
                                ( { model
                                    | page = Page.Graph
                                    , graph = graph
                                    , url = url
                                  }
                                , List.map GraphEffect (Graph.GetSvgElementEffect :: graphEffect)
                                )

                    _ ->
                        n model
            )
            (Route.parse plugins routeConfig model.url)
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
            }
                |> Authorized
    }
