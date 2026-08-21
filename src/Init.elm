module Init exposing (init, viewConfigFromSettings)

import Config exposing (config)
import Config.Update as Update
import Config.UserSettings
import Config.View exposing (characterDimensionsDecoder)
import Dict exposing (Dict)
import Effect.Api
import Init.Locale as Locale
import Init.Notification as Notification
import Init.Pathfinder as Pathfinder
import Init.Search as Search
import Init.Statusbar as Statusbar
import Json.Decode
import Model exposing (..)
import Model.Locale as Locale
import Plugin.Update as Plugin
import RemoteData exposing (RemoteData(..))
import Tuple exposing (first)
import Update exposing (updateByPluginOutMsg)
import Url exposing (Url)
import Util.ThemedSelectBox as TSelectBox


init : Update.Config -> Flags -> Url -> key -> ( Model key, List Effect )
init uc flags url key =
    let
        settings =
            flags.localStorage
                |> Json.Decode.decodeValue Config.UserSettings.decoder
                |> Result.withDefault (Config.UserSettings.default flags.locale)

        cd =
            flags.characterDimensions
                |> Json.Decode.decodeValue characterDimensionsDecoder
                |> Result.withDefault Dict.empty

        ( locale, localeEffect ) =
            Locale.init settings

        ( pluginStates, outMsgs, cmd ) =
            Plugin.init flags.pluginFlags

        ( pathfinderState, pathfinderCmd ) =
            Pathfinder.init settings
    in
    ( { url = url
      , key = key
      , config = viewConfigFromSettings locale cd settings
      , page = Home
      , search = Search.initWithRecents (Search.initSearchAddressAndTxs Nothing) settings.recentSearches
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
      , navbarSubMenu = Nothing
      , fileDragOver = False
      }
    , List.map LocaleEffect localeEffect
        ++ [ Effect.Api.GetConceptsEffect "entity" BrowserGotEntityTaxonomy
                |> ApiEffect
           , Effect.Api.GetConceptsEffect "abuse" BrowserGotAbuseTaxonomy
                |> ApiEffect
           , PluginEffect cmd
           , CmdEffect (pathfinderCmd |> Cmd.map PathfinderMsg)
           ]
    )
        |> getStatistics
        |> updateByPluginOutMsg uc outMsgs


getStatistics : ( Model key, List Effect ) -> ( Model key, List Effect )
getStatistics ( model, eff ) =
    if model.stats == NotAsked then
        ( { model | stats = RemoteData.Loading }
        , ApiEffect (Effect.Api.GetStatisticsEffect BrowserGotStatistics) :: eff
        )

    else
        ( model, eff )


{-| Restore the view config from the settings the last session saved.

Split out of `init` so it can be tested: `init` takes a `Plugin.Model.Flags`
record whose shape is generated per registered plugin, so no value of that type
can be written portably and the function cannot be called from a test at all.
This part needs none of it.

Worth keeping honest -- a field that is persisted by
`Model.userSettingsFromMainModel` but hardcoded here is saved on every change and
then silently dropped at the next boot, which is what `showBothValues` did.

-}
viewConfigFromSettings : Locale.Model -> Dict String Config.View.CharacterDimension -> Config.UserSettings.UserSettings -> Config.View.Config
viewConfigFromSettings locale characterDimensions settings =
    { locale = locale
    , theme = config.theme
    , lightmode = settings.lightMode |> Maybe.withDefault True
    , size = Nothing
    , showDatesInUserLocale = settings.showDatesInUserLocale |> Maybe.withDefault True
    , showTimeZoneOffset = settings.showTimeZoneOffset |> Maybe.withDefault False
    , showTimestampOnTxEdge = settings.showTimestampOnTxEdge |> Maybe.withDefault True
    , showValuesInFiat = settings.showValuesInFiat |> Maybe.withDefault False
    , preferredFiatCurrency = settings.preferredFiatCurrency |> Maybe.withDefault "usd"
    , showHash = settings.showHash |> Maybe.withDefault False
    , showLabelsInTaggingOverview = False
    , allConcepts = []
    , abuseConcepts = []
    , showConversionEdges = True
    , characterDimensions = characterDimensions
    , showBothValues = settings.showBothValues |> Maybe.withDefault False
    }
