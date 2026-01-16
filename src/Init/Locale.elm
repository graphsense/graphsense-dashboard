module Init.Locale exposing (init)

import Config.UserSettings exposing (UserSettings)
import DateFormat
import Dict
import Effect.Locale exposing (Effect(..))
import Locale.English
import Maybe
import Model.Locale exposing (..)
import Msg.Locale exposing (Msg(..))
import Numeral
import Time
import Update.Locale exposing (switch)


init : UserSettings -> ( Model, List Effect )
init uc =
    let
        locale =
            uc.selectedLanguage

        fetchTimezone =
            uc.showDatesInUserLocale |> Maybe.withDefault True
    in
    ( { mapping = Empty
      , locale = locale
      , numberFormat = Numeral.format
      , valueDetail = uc.valueDetail |> Maybe.withDefault Magnitude
      , zone = Time.utc
      , timeLang = DateFormat.english
      , unitToString = Locale.English.unitToString
      , supportedTokens = Dict.empty
      }
        |> switch locale
    , Effect.Locale.getTranslationEffect locale
        :: (if fetchTimezone then
                [ GetTimezoneEffect BrowserSentTimezone ]

            else
                []
           )
    )
