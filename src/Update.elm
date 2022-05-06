module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Effect exposing (n)
import Effect.Locale as Locale
import Http exposing (Error(..))
import Model exposing (..)
import Page
import RecordSetter exposing (..)
import RemoteData as RD
import Route
import Update.Graph as Graph
import Update.Graph.Adding as Adding
import Update.Locale as Locale
import Update.Search as Search
import Update.Store as Store
import Url exposing (Url)


update : Msg -> Model key -> ( Model key, List Effect )
update msg model =
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
            updateByUrl url model

        BrowserGotStatistics result ->
            case result of
                Ok stats ->
                    n { model | stats = RD.Success stats }

                Err error ->
                    n { model | stats = RD.Failure error }

        BrowserGotResponseWithHeaders result ->
            case result of
                Ok ( headers, message ) ->
                    update message
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
            ( { model | locale = model.locale |> s_locale locale }
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

        LocaleMsg m ->
            let
                ( locale, localeEffects ) =
                    Locale.update m model.locale
            in
            ( { model | locale = locale }
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

        GraphMsg m ->
            let
                ( graph, graphEffects ) =
                    Graph.update m model.graph
            in
            ( { model | graph = graph }
            , List.map GraphEffect graphEffects
            )

        StoreMsg m ->
            let
                ( store, storeEffects ) =
                    Store.update m model.store
            in
            ( { model | store = store }
            , List.map StoreEffect storeEffects
            )


updateByUrl : Url -> Model key -> ( Model key, List Effect )
updateByUrl url model =
    let
        routeConfig =
            model.stats
                |> RD.map (.currencies >> List.map .name)
                |> RD.withDefault []
                |> (\c -> { currencies = c })
    in
    Route.parse routeConfig url
        |> Maybe.map
            (\route ->
                case route of
                    Route.Currency curr (Route.Address a) ->
                        case Store.getAddress { currency = curr, address = a } model.store of
                            Store.Found address ->
                                let
                                    ( graph, graphEffect ) =
                                        Graph.addAddress address model.graph
                                in
                                ( { model
                                    | page = Page.Graph
                                    , graph = graph
                                  }
                                , List.map GraphEffect graphEffect
                                )

                            Store.NotFound effect ->
                                let
                                    ( graph, graphEffect ) =
                                        Graph.addingAddress { currency = curr, address = a } model.graph
                                in
                                ( { model
                                    | page = Page.Graph
                                    , graph = graph
                                  }
                                , List.map GraphEffect graphEffect
                                    ++ List.map StoreEffect effect
                                )

                    _ ->
                        n model
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
            }
                |> Authorized
    }
