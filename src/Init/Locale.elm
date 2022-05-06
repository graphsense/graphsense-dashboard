module Init.Locale exposing (init)

import DateFormat.Language
import Dict
import Effect.Locale exposing (Effect(..))
import FormatNumber.Locales
import Http
import Locale.German
import Model.Locale as Model exposing (..)
import Msg.Locale exposing (Msg(..))
import Time


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
      , currency = Coin
      }
    , [ Effect.Locale.getTranslationEffect locale
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
