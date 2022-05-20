module Update exposing (update, updateByUrl)

import Browser
import Browser.Navigation as Nav
import Config.Update exposing (Config)
import Dict exposing (Dict)
import Effect exposing (n)
import Effect.Graph as Graph
import Effect.Locale as Locale
import Http exposing (Error(..))
import Log
import Model exposing (..)
import Model.Graph.Id as Id
import Msg.Graph as Graph
import Page
import Plugin
import RecordSetter exposing (..)
import RemoteData as RD
import Route
import Tuple exposing (..)
import Update.Graph as Graph
import Update.Graph.Adding as Adding
import Update.Locale as Locale
import Update.Search as Search
import Url exposing (Url)


update : Config -> Msg -> Model key -> ( Model key, List Effect )
update uc msg model =
    case msg of
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
            updateByUrl uc url model

        BrowserGotStatistics result ->
            case result of
                Ok stats ->
                    updateByUrl uc model.url { model | stats = RD.Success stats }

                Err error ->
                    n { model | stats = RD.Failure error }

        BrowserGotResponseWithHeaders result ->
            case result of
                Ok ( headers, message ) ->
                    update uc
                        message
                        { model
                            | user =
                                updateRequestLimit headers model.user
                        }

                Err ( BadStatus 401, eff ) ->
                    { model
                        | user =
                            model.user
                                |> s_auth
                                    (case model.user.auth of
                                        Unauthorized effs ->
                                            Unauthorized <| effs ++ [ eff ]

                                        _ ->
                                            Unauthorized [ eff ]
                                    )
                    }
                        |> n

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
                | user = model.user |> s_hovercardElement Nothing
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
            let
                effs =
                    case model.user.auth of
                        Unauthorized effects ->
                            effects

                        _ ->
                            []
            in
            ( { model
                | user =
                    model.user
                        |> s_auth
                            (if List.isEmpty effs then
                                Unknown

                             else
                                Loading
                            )
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
            let
                ( search, searchEffects ) =
                    Search.update m model.search
            in
            ( { model | search = search }
            , List.map SearchEffect searchEffects
            )

        GraphMsg (Graph.PluginMsg pid place value) ->
            Dict.get pid model.config.plugins
                |> Maybe.map2
                    (\state plugin ->
                        case place of
                            Plugin.Model ->
                                let
                                    ( newState, cmd ) =
                                        plugin.update.model value state
                                in
                                ( { model
                                    | plugins = Dict.insert pid newState model.plugins
                                  }
                                , [ PluginEffect pid place cmd
                                  ]
                                )

                            Plugin.Address ->
                                let
                                    ( newState, cmd ) =
                                        plugin.update.graph.address value state
                                in
                                ( { model
                                    | graph = model.graph

                                    {- Layer.updateAddress
                                       { address
                                           | plugins = Dict.insert pid newState address.plugins
                                       }
                                    -}
                                  }
                                , [ PluginEffect pid place cmd
                                  ]
                                )
                    )
                    (Dict.get pid model.plugins)
                |> Maybe.withDefault (n model)

        GraphMsg m ->
            let
                ( graph, graphEffects ) =
                    Graph.update uc m model.graph
            in
            ( { model | graph = graph }
            , List.map GraphEffect graphEffects
            )


updateByUrl : Config -> Url -> Model key -> ( Model key, List Effect )
updateByUrl uc url model =
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
                    Route.Graph graphRoute ->
                        let
                            ( graph, graphEffect ) =
                                Graph.updateByRoute graphRoute model.graph
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
            (Route.parse routeConfig model.url)
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
