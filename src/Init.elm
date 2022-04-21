module Init exposing (init)

import Config exposing (config)
import Effect exposing (Effect(..), n)
import Locale.Init as Locale
import Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)


init : Flags -> Url -> key -> ( Model key, Effect )
init flags url key =
    let
        ( locale, localeEffect ) =
            Locale.init
                { locale = flags.locale
                }
    in
    ( { url = url
      , key = key
      , locale = locale
      , search = ()
      , user = ()
      , stats = NotAsked
      }
    , LocaleEffect localeEffect
    )
        |> getStatistics


getStatistics : ( Model key, Effect ) -> ( Model key, Effect )
getStatistics ( model, eff ) =
    if model.stats == NotAsked then
        ( { model | stats = Loading }
        , Effect.batch [ eff, GetStatisticsEffect ]
        )

    else
        ( model, eff )
