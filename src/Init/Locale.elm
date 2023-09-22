module Init.Locale exposing (init)

import Config.UserSettings exposing (UserSettings)
import DateFormat.Language
import DateFormat.Relative
import Effect.Locale exposing (Effect(..))
import Locale.English
import Model.Currency exposing (..)
import Model.Locale as Model exposing (..)
import Msg.Locale exposing (Msg(..))
import Numeral
import Time
import Update.Locale exposing (switch)


init : UserSettings -> ( Model, List Effect )
init uc =
    let
        locale =
            uc.selectedLanguage
    in
    ( { mapping = Empty
      , locale = locale
      , numberFormat = Numeral.format
      , valueDetail = uc.valueDetail |> Maybe.withDefault Magnitude
      , zone = Time.utc
      , timeLang = DateFormat.Language.english
      , currency = uc.valueDenomination |> Maybe.withDefault Coin
      , relativeTimeOptions = DateFormat.Relative.defaultRelativeOptions
      , unitToString = Locale.English.unitToString
      , supportedTokens = Nothing
      }
        |> switch locale
    , [ Effect.Locale.getTranslationEffect locale
      , GetTimezoneEffect BrowserSentTimezone
      ]
    )
