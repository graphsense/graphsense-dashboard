module Update.Locale exposing (changeTimeZone, changeValueDetail, registerSwapAsset, setSupportedTokens, switch, update)

import Api.Data
import DateFormat
import Dict
import Effect.Locale exposing (Effect)
import Languages.German
import Languages.Italian
import Languages.Spanish
import Model.Locale exposing (..)
import Msg.Locale exposing (Msg(..))
import Numeral
import Time
import Util exposing (n)


duration : Float
duration =
    700


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        BrowserSentTimezone zone ->
            n { model | zone = zone }

        BrowserLoadedTranslation _ result ->
            result
                |> Result.map
                    (Dict.foldl
                        (\k ->
                            Dict.insert
                                ((String.left 1 k
                                    |> String.toLower
                                 )
                                    ++ String.dropLeft 1 k
                                )
                        )
                        Dict.empty
                    )
                |> Result.map
                    (\mapping ->
                        { model
                            | mapping =
                                case model.mapping of
                                    Empty ->
                                        Settled mapping

                                    Transition start _ delta ->
                                        Transition start mapping delta

                                    Settled end ->
                                        Transition end mapping 0
                        }
                    )
                |> Result.withDefault model
                |> n

        RuntimeTick delta ->
            case model.mapping of
                Transition start end curr ->
                    let
                        now =
                            curr
                                + delta
                                / duration
                    in
                    { model
                        | mapping =
                            if now > 1 then
                                Settled end

                            else
                                Transition start end now
                    }
                        |> n

                _ ->
                    n model


switch : String -> Model -> Model
switch locale model =
    { model
        | locale = locale
        , numberFormat =
            case locale of
                "de" ->
                    Numeral.formatWithLanguage Languages.German.lang

                "it" ->
                    Numeral.formatWithLanguage Languages.Italian.lang

                "es" ->
                    Numeral.formatWithLanguage Languages.Spanish.lang

                _ ->
                    Numeral.format
        , timeLang =
            case locale of
                "de" ->
                    DateFormat.german

                "it" ->
                    DateFormat.italian

                "es" ->
                    DateFormat.spanish

                _ ->
                    DateFormat.english
    }


changeTimeZone : Time.Zone -> Model -> Model
changeTimeZone tz m =
    { m
        | zone = tz
    }


changeValueDetail : String -> Model -> Model
changeValueDetail curr model =
    { model
        | valueDetail =
            case String.toLower curr of
                "magnitude" ->
                    Magnitude

                _ ->
                    Exact
    }


setSupportedTokens : Api.Data.TokenConfigs -> String -> Model -> Model
setSupportedTokens configs currency model =
    { model | supportedTokens = Dict.insert currency configs model.supportedTokens }


{-| Keeps the first registration of a contract; the address is compared
case-insensitively (the API serves checksummed, swap legs lowercased).
-}
registerSwapAsset : String -> Api.Data.TokenConfig -> Model -> Model
registerSwapAsset network config model =
    case config.contractAddress of
        Nothing ->
            model

        Just address ->
            let
                known =
                    Dict.get network model.swapAssets
                        |> Maybe.withDefault []

                sameContract tc =
                    Maybe.map String.toLower tc.contractAddress == Just (String.toLower address)
            in
            if List.any sameContract known then
                model

            else
                { model | swapAssets = Dict.insert network (config :: known) model.swapAssets }
