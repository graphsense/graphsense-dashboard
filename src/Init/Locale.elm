module Init.Locale exposing (init)

import DateFormat.Language
import DateFormat.Relative
import Dict
import Effect.Locale exposing (Effect(..))
import Http
import Languages.German
import Locale.English
import Locale.German
import Model.Currency exposing (..)
import Model.Locale as Model exposing (..)
import Msg.Locale exposing (Msg(..))
import Numeral
import Time
import Update.Locale exposing (switch)


init : Flags -> ( Model, List Effect )
init { locale } =
    ( { mapping = Empty
      , locale = locale
      , numberFormat = Numeral.format
      , valueDetail = Magnitude
      , zone = Time.utc
      , timeLang = DateFormat.Language.english
      , currency = Coin
      , relativeTimeOptions = DateFormat.Relative.defaultRelativeOptions
      , unitToString = Locale.English.unitToString
      , supportedTokens = Nothing
      }
        |> switch locale
    , [ Effect.Locale.getTranslationEffect locale
      , GetTimezoneEffect BrowserSentTimezone
      ]
    )
