module Init exposing (init)

import CaseMgm
import Config exposing (config)
import Dict
import Init.Graph as Graph
import Init.Locale as Locale
import Init.Search as Search
import Model exposing (..)
import Page
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)


init : Flags -> Url -> key -> ( Model key, List Effect )
init flags url key =
    let
        ( locale, localeEffect ) =
            Locale.init
                { locale = flags.locale
                }
    in
    ( { url = url
      , key = key
      , config =
            { locale = locale
            , theme = config.theme
            , plugins =
                Dict.fromList
                    [ ( "casemgm", CaseMgm.plugin ) ]
            }
      , locale = locale
      , page = Page.Stats
      , search = Search.init
      , graph = Graph.init flags.now
      , user =
            { apiKey = ""
            , auth = Unknown
            , hovercardElement = Nothing
            }
      , stats = NotAsked
      , width = flags.width
      , height = flags.height
      , error = ""
      , plugins = Dict.empty
      }
    , List.map LocaleEffect localeEffect
    )
        |> getStatistics


getStatistics : ( Model key, List Effect ) -> ( Model key, List Effect )
getStatistics ( model, eff ) =
    if model.stats == NotAsked then
        ( { model | stats = RemoteData.Loading }
        , GetStatisticsEffect :: eff
        )

    else
        ( model, eff )
