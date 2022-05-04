module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http exposing (Error(..))
import Locale.Effect as Locale
import Locale.Update as Locale
import Model exposing (..)
import RecordSetter exposing (..)
import RemoteData as RD
import Search.Update as Search
import Url exposing (Url)


update : Msg -> Model key -> ( Model key, Effect )
update msg model =
    case msg of
        UserRequestsUrl request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Url.toString url
                        |> NavPushUrlEffect
                    )

                Browser.External url ->
                    ( model
                    , NavLoadEffect url
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
            , BatchedEffects effs
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
                ( locale, localeEffect ) =
                    Locale.update m model.locale
            in
            ( { model | locale = locale }
            , LocaleEffect localeEffect
            )

        SearchMsg m ->
            let
                ( search, searchEffect ) =
                    Search.update m model.search
            in
            ( { model | search = search }
            , SearchEffect searchEffect
            )


updateByUrl : Url -> Model key -> ( Model key, Effect )
updateByUrl _ model =
    n model


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
