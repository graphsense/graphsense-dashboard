module Update.Locale exposing (changeCurrency, changeTimeZone, changeValueDetail, setSupportedTokens, switch, update)

import Api.Data
import DateFormat.Language
import DateFormat.Relative
import Dict
import Effect.Locale exposing (Effect)
import Languages.German
import Locale.English
import Locale.German
import Model.Currency exposing (..)
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
                                        Transition Dict.empty mapping 0

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

                _ ->
                    Numeral.format
        , timeLang =
            case locale of
                "de" ->
                    Locale.German.german

                _ ->
                    DateFormat.Language.english
        , relativeTimeOptions =
            case locale of
                "de" ->
                    Locale.German.relativeTimeOptions

                _ ->
                    DateFormat.Relative.defaultRelativeOptions
        , unitToString =
            case locale of
                "de" ->
                    Locale.German.unitToString

                _ ->
                    Locale.English.unitToString
    }


changeCurrency : String -> Model -> Model
changeCurrency curr model =
    { model
        | currency =
            case String.toLower curr of
                "coin" ->
                    Coin

                fiat ->
                    Fiat fiat
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
