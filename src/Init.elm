module Init exposing (init)

import Config exposing (config)
import Config.UserSettings
import Effect.Api
import Init.Graph as Graph
import Init.Locale as Locale
import Init.Search as Search
import Init.Statusbar as Statusbar
import Json.Decode
import Model exposing (..)
import Plugin.Update as Plugin exposing (Plugins)
import RemoteData exposing (RemoteData(..))
import Update exposing (updateByPluginOutMsg)
import Url exposing (Url)


init : Plugins -> Flags -> Url -> key -> ( Model key, List Effect )
init plugins flags url key =
    let
        settings =
            Json.Decode.decodeValue Config.UserSettings.decoder flags.settings |> Result.withDefault Config.UserSettings.default

        ( locale, localeEffect ) =
            Locale.init settings

        ( pluginStates, outMsgs, cmd ) =
            Plugin.init plugins flags.pluginFlags
    in
    ( { url = url
      , key = key
      , config =
            { locale = locale
            , theme = config.theme
            , lightmode = settings.lightMode |> Maybe.withDefault True
            , size = Nothing
            }
      , locale = locale
      , page = Stats
      , search = Search.init
      , graph = Graph.init settings flags.now
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
      , supportedTokens = Nothing
      , dialog = Nothing
      , plugins = pluginStates
      , dirty = False
      }
    , List.map LocaleEffect localeEffect
        ++ [ Effect.Api.GetConceptsEffect "entity" BrowserGotEntityTaxonomy
                |> ApiEffect
           , Effect.Api.GetConceptsEffect "abuse" BrowserGotAbuseTaxonomy
                |> ApiEffect
           , Effect.Api.ListSupportedTokensEffect BrowserGotSupportedTokens
                |> ApiEffect
           , PluginEffect cmd
           ]
    )
        |> getStatistics
        |> updateByPluginOutMsg plugins outMsgs


getStatistics : ( Model key, List Effect ) -> ( Model key, List Effect )
getStatistics ( model, eff ) =
    if model.stats == NotAsked then
        ( { model | stats = RemoteData.Loading }
        , ApiEffect (Effect.Api.GetStatisticsEffect BrowserGotStatistics) :: eff
        )

    else
        ( model, eff )
