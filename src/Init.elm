module Init exposing (init)

import Config exposing (config)
import Dict
import Init.Graph as Graph
import Init.Locale as Locale
import Init.Search as Search
import Init.Statusbar as Statusbar
import Model exposing (..)
import Plugin.Update as Plugin exposing (Plugins)
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)


init : Plugins -> Flags -> Url -> key -> ( Model key, List Effect )
init plugins flags url key =
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
            }
      , locale = locale
      , page = Stats
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
      , statusbar = Statusbar.init
      , dialog = Nothing
      , plugins = Plugin.init plugins
      }
    , List.map LocaleEffect localeEffect
        ++ [ GetConceptsEffect "entity" BrowserGotEntityTaxonomy
           , GetConceptsEffect "abuse" BrowserGotAbuseTaxonomy
           ]
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
