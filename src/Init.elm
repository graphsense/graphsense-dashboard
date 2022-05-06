module Init exposing (init)

import Config exposing (config)
import Graph.Init as Graph
import Locale.Init as Locale
import Model exposing (..)
import Page
import RemoteData exposing (RemoteData(..))
import Search.Init as Search
import Store.Init as Store
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
      , page = Page.Stats
      , search = Search.init
      , graph = Graph.init
      , store = Store.init
      , user =
            { apiKey = ""
            , auth = Unknown
            , hovercardElement = Nothing
            }
      , stats = NotAsked
      }
    , LocaleEffect localeEffect
    )
        |> getStatistics


getStatistics : ( Model key, Effect ) -> ( Model key, Effect )
getStatistics ( model, eff ) =
    if model.stats == NotAsked then
        ( { model | stats = RemoteData.Loading }
        , BatchedEffects [ eff, GetStatisticsEffect ]
        )

    else
        ( model, eff )
