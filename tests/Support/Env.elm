module Support.Env exposing
    ( updateConfig
    , updatePlugins
    , viewConfig
    , viewPlugins
    , viewport
    )

{-| The configuration both headless harnesses run against, in one place so a
Pathfinder scenario and a whole-app scenario cannot drift apart.

Everything here is fixed: a 1920x1080 viewport, the "en" locale with no
translations loaded, and no plugins. Nothing depends on the machine the tests
run on or on what `config/Config.elm` happens to register locally.

-}

import Config
import Config.Update
import Config.UserSettings
import Config.View
import Dict
import Init.Locale
import Model.Graph.Coords exposing (BBox)
import Model.Locale
import Plugin
import Plugin.Update
import Plugin.View


{-| `Plugin.Update.empty` rather than `Plugin.updatePlugins Config.plugins`: the
update path must not depend on the local plugin configuration.
-}
updatePlugins : Plugin.Update.Plugins
updatePlugins =
    Plugin.Update.empty


{-| The view side has no `empty`, but every plugin view is keyed off
`Plugin.Model.emptyModelState`, so none of them contributes anything.
-}
viewPlugins : Plugin.View.Plugins
viewPlugins =
    Plugin.viewPlugins Config.plugins


{-| `Init.Locale.init` leaves `mapping` empty — translations are fetched over
HTTP — so `View.Locale.string` falls back to the key. That keeps assertions on
rendered text readable: the key _is_ the English text.
-}
locale : Model.Locale.Model
locale =
    Config.UserSettings.default "en"
        |> Init.Locale.init
        |> Tuple.first


{-| `View.Pathfinder.graph` renders nothing at all when `size` is `Nothing`.
-}
viewport : BBox
viewport =
    { x = 0, y = 0, width = 1920, height = 1080 }


{-| Same shape `Main.main` builds.
-}
updateConfig : Config.Update.Config
updateConfig =
    { defaultColor = Config.config.theme.graph.defaultColor
    , categoryToColor = Config.config.theme.graph.categoryToColor
    , highlightsColorScheme = Config.config.theme.graph.highlightsColorScheme
    , locale = locale
    , size = Just viewport
    , allConcepts = []
    , abuseConcepts = []
    }


viewConfig : Config.View.Config
viewConfig =
    { theme = Config.config.theme
    , locale = locale
    , lightmode = True
    , size = Just viewport
    , showDatesInUserLocale = True
    , showTimeZoneOffset = False
    , showTimestampOnTxEdge = True
    , preferredFiatCurrency = "usd"
    , showValuesInFiat = False
    , showHash = False
    , showLabelsInTaggingOverview = False
    , showConversionEdges = True
    , allConcepts = []
    , abuseConcepts = []
    , characterDimensions = Dict.empty
    , showBothValues = False
    }
