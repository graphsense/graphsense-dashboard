module Update exposing (update, updateByUrl)

import Browser
import Browser.Navigation as Nav
import Config.Update exposing (Config)
import Dict exposing (Dict)
import Effect exposing (n)
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Store as Store
import Http exposing (Error(..))
import Model exposing (..)
import Msg.Graph as Graph
import Msg.Store as Store
import Page
import RecordSetter exposing (..)
import RemoteData as RD
import Route
import Tuple exposing (..)
import Update.Graph as Graph
import Update.Graph.Adding as Adding
import Update.Locale as Locale
import Update.Search as Search
import Update.Store as Store
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

        GraphMsg m ->
            let
                ( graph, graphEffects ) =
                    Graph.update uc m model.graph
            in
            ( { model | graph = graph }
            , List.map GraphEffect graphEffects
            )

        StoreMsg (Store.BrowserGotAddress address) ->
            let
                ( store, storeEffects ) =
                    Store.update (Store.BrowserGotAddress address) model.store

                ( newStore, retrieved ) =
                    Store.getEntity
                        { currency = address.currency
                        , entity = address.entity
                        , forAddress = address.address
                        }
                        store

                ( graph, effects ) =
                    case retrieved of
                        Store.Found entity ->
                            Graph.addAddressAndEntity uc address entity model.graph
                                |> mapSecond (List.map GraphEffect)

                        Store.NotFound eff ->
                            Graph.addAddress uc address model.graph
                                |> mapSecond (List.map GraphEffect)
                                |> mapSecond ((++) (List.map StoreEffect eff))
            in
            ( { model
                | store = newStore
                , graph = graph
              }
            , List.map StoreEffect storeEffects
                ++ effects
            )

        StoreMsg (Store.BrowserGotEntity a entity) ->
            let
                ( store, storeEffects ) =
                    Store.update (Store.BrowserGotEntity a entity) model.store

                ( newStore, retrieved ) =
                    Store.getAddress { currency = entity.currency, address = a } store

                ( graph, effects ) =
                    case retrieved of
                        Store.Found address ->
                            Graph.addAddressAndEntity uc address entity model.graph
                                |> mapSecond (List.map GraphEffect)

                        Store.NotFound eff ->
                            Graph.addEntity uc entity model.graph
                                |> mapSecond (List.map GraphEffect)
                                |> mapSecond ((++) (List.map StoreEffect eff))
            in
            ( { model
                | store = newStore
                , graph = graph
              }
            , List.map StoreEffect storeEffects
                ++ effects
            )

        StoreMsg (Store.BrowserGotEntityForAddress a entity) ->
            update uc (Store.BrowserGotEntity a entity |> StoreMsg) model


updateByUrl : Config -> Url -> Model key -> ( Model key, List Effect )
updateByUrl uc url model =
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
                        let
                            ( store, retrieved ) =
                                Store.getAddress { currency = curr, address = a } model.store

                            ( ( graph, graphEffect ), ( newStore_, storeEffect ) ) =
                                case retrieved of
                                    Store.Found address ->
                                        let
                                            ( newStore, retr2 ) =
                                                Store.getEntity
                                                    { currency = curr
                                                    , entity = address.entity
                                                    , forAddress = address.address
                                                    }
                                                    store
                                        in
                                        case retr2 of
                                            Store.Found entity ->
                                                ( Graph.addAddressAndEntity uc address entity model.graph
                                                , ( newStore, [] )
                                                )

                                            Store.NotFound effect ->
                                                ( Graph.addingEntity { currency = curr, entity = address.entity } model.graph
                                                , ( newStore, effect )
                                                )

                                    Store.NotFound effect ->
                                        ( Graph.addingAddress { currency = curr, address = a } model.graph
                                        , ( store
                                          , Store.GetEntityForAddressEffect
                                                { address = a
                                                , currency = curr
                                                , toMsg = Store.BrowserGotEntityForAddress a
                                                }
                                                :: effect
                                          )
                                        )
                        in
                        ( { model
                            | page = Page.Graph
                            , graph = graph
                            , store = newStore_
                          }
                        , List.map GraphEffect (Graph.GetSvgElementEffect :: graphEffect)
                            ++ List.map StoreEffect storeEffect
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
