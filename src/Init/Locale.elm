module Init.Locale exposing (init)

import DateFormat.Language
import Dict
import Effect.Locale exposing (Effect(..))
import Http
import Languages.German
import Locale.German
import Model.Locale as Model exposing (..)
import Msg.Locale exposing (Msg(..))
import Numeral
import Time


init : Flags -> ( Model, List Effect )
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
    )


initNumberFormat : String -> String -> Float -> String
initNumberFormat locale =
    case locale of
        "de" ->
            Numeral.formatWithLanguage Languages.German.lang

        _ ->
            Numeral.format
