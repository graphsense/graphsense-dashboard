module Init exposing (init)

import Config exposing (config)
import Init.Graph as Graph
import Init.Locale as Locale
import Init.Search as Search
import Init.Store as Store
import Model exposing (..)
import Page
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
