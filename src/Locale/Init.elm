module Locale.Init exposing (init)

import DateFormat.Language
import Dict
import FormatNumber.Locales
import Http
import Locale.Effect exposing (Effect(..))
import Locale.German
import Locale.Model as Model exposing (..)
import Locale.Msg exposing (Msg(..))
import Time
import Yaml.Decode exposing (dict, fromString, string)


init : Flags -> ( Model, Effect )
init { locale } =
    ( { mapping = Empty
      , locale = locale
      , numberFormat = initNumberFormat locale
      , zone = Time.utc
      , timeLang =
            case locale of
                "de" ->
                    Locale.German.german

                _ ->
                    DateFormat.Language.english
      }
    , [ getTranslationEffect locale
      , GetTimezoneEffect BrowserSentTimezone
      ]
        |> BatchEffect
    )


initNumberFormat : String -> FormatNumber.Locales.Locale
initNumberFormat locale =
    case locale of
        "de" ->
            { decimals = FormatNumber.Locales.Exact 2
            , system = FormatNumber.Locales.Western
            , thousandSeparator = "."
            , decimalSeparator = ","
            , negativePrefix = "-"
            , negativeSuffix = ""
            , positivePrefix = ""
            , positiveSuffix = ""
            , zeroPrefix = ""
            , zeroSuffix = ""
            }

        _ ->
            FormatNumber.Locales.usLocale


getTranslationEffect : String -> Effect
getTranslationEffect locale =
    { url = "/lang/" ++ locale ++ ".yaml"
    , toMsg =
        Result.andThen
            (fromString (dict string)
                >> Result.mapError toHttpError
            )
            >> BrowserLoadedTranslation locale
    }
        |> GetTranslationEffect


toHttpError : Yaml.Decode.Error -> Http.Error
toHttpError err =
    case err of
        Yaml.Decode.Parsing e ->
            "Error when parsing YAML: " ++ e |> Http.BadBody

        Yaml.Decode.Decoding e ->
            "Error when decoding YAML: " ++ e |> Http.BadBody
