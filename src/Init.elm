module Init exposing (init)

import Config exposing (config)
import Config.Update as Update
import Config.UserSettings
import Config.View exposing (characterDimensionsDecoder)
import Dict
import Effect.Api
import Init.Graph as Graph
import Init.Locale as Locale
import Init.Notification as Notification
import Init.Pathfinder as Pathfinder
import Init.Search as Search
import Init.Statusbar as Statusbar
import Json.Decode
import Model exposing (..)
import Model.Locale as Locale
import Plugin.Update as Plugin exposing (Plugins)
import RemoteData exposing (RemoteData(..))
import Tuple exposing (first)
import Update exposing (updateByPluginOutMsg)
import Url exposing (Url)
import Util.ThemedSelectBox as TSelectBox


init : Plugins -> Update.Config -> Flags -> Url -> key -> ( Model key, List Effect )
init plugins uc flags url key =
    let
        settings =
            flags.localStorage
                |> Json.Decode.decodeValue Config.UserSettings.decoder
                |> Result.withDefault Config.UserSettings.default

        cd =
            flags.characterDimensions
                |> Json.Decode.decodeValue characterDimensionsDecoder
                |> Result.withDefault Dict.empty

        ( locale, localeEffect ) =
            Locale.init settings

        ( pluginStates, outMsgs, cmd ) =
            Plugin.init plugins flags.pluginFlags

        ( pathfinderState, pathfinderCmd ) =
            Pathfinder.init settings
    in
    ( { url = url
      , key = key
      , config =
            { locale = locale
            , theme = config.theme
            , lightmode = settings.lightMode |> Maybe.withDefault True
            , size = Nothing
            , showDatesInUserLocale = settings.showDatesInUserLocale |> Maybe.withDefault True
            , showTimeZoneOffset = settings.showTimeZoneOffset |> Maybe.withDefault False
            , showTimestampOnTxEdge = settings.showTimestampOnTxEdge |> Maybe.withDefault True
            , showValuesInFiat = settings.showValuesInFiat |> Maybe.withDefault False
            , preferredFiatCurrency = settings.preferredFiatCurrency |> Maybe.withDefault "usd"
            , showLabelsInTaggingOverview = False
            , allConcepts = []
            , abuseConcepts = []
            , characterDimensions = cd
            }
      , page = Home
      , search = Search.init (Search.initSearchAddressAndTxs Nothing)
      , graph = Graph.init settings flags.now
      , pathfinder = pathfinderState
      , user =
            { apiKey = ""
            , auth = Unknown
            , hovercard = Nothing
            }
      , stats = NotAsked
      , width = flags.width
      , height = flags.height
      , error = ""
      , statusbar = Statusbar.init
      , supportedTokens = Dict.empty
      , dialog = Nothing
      , plugins = pluginStates
      , dirty = False
      , notifications = Notification.init
      , localeSelectBox = TSelectBox.init <| List.map first Locale.locales
      , tooltip = Nothing
      , navbarSubMenu = Nothing
      }
    , List.map LocaleEffect localeEffect
        ++ [ Effect.Api.GetConceptsEffect "entity" BrowserGotEntityTaxonomy
                |> ApiEffect
           , Effect.Api.GetConceptsEffect "abuse" BrowserGotAbuseTaxonomy
                |> ApiEffect
           , Effect.Api.ListSupportedTokensEffect "eth" (BrowserGotSupportedTokens "eth")
                |> ApiEffect
           , Effect.Api.ListSupportedTokensEffect "trx" (BrowserGotSupportedTokens "trx")
                |> ApiEffect
           , PluginEffect cmd
           , CmdEffect (pathfinderCmd |> Cmd.map PathfinderMsg)
           ]
    )
        |> getStatistics
        |> updateByPluginOutMsg plugins uc outMsgs


getStatistics : ( Model key, List Effect ) -> ( Model key, List Effect )
getStatistics ( model, eff ) =
    if model.stats == NotAsked then
        ( { model | stats = RemoteData.Loading }
        , ApiEffect (Effect.Api.GetStatisticsEffect BrowserGotStatistics) :: eff
        )

    else
        ( model, eff )
