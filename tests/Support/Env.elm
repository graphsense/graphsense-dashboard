module Support.Env exposing
    ( updateConfig
    , viewConfig
    , viewport
    )

{-| The configuration both headless harnesses run against, in one place so a
Pathfinder scenario and a whole-app scenario cannot drift apart.

Everything here is fixed: a 1920x1080 viewport, the "en" locale with no
translations loaded, the statistics answered (`servedNetworks`), and no
plugins. Nothing depends on the machine the tests run on or on what
`config/Config.elm` happens to register locally.

-}

import Config.Update
import Config.UserSettings
import Config.View
import Dict
import Init.Locale
import Model.Graph.Coords exposing (BBox)
import Model.Locale
import Model.ThemeMode


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
    { locale = locale
    , size = Just viewport
    , allConcepts = []
    , abuseConcepts = []
    , networks = servedNetworks
    , preferredFiatCurrency = "usd"
    }


{-| The networks the statistics list once they have arrived: the core
networks and the lite ones. A scenario starts after boot (see the module
doc), so the statistics are in.
-}
servedNetworks : List String
servedNetworks =
    [ "btc", "bch", "ltc", "zec", "eth", "trx", "bnb", "arb", "base", "opt", "pol", "avax", "gnosis", "robinhood" ]


viewConfig : Config.View.Config
viewConfig =
    { locale = locale
    , lightmode = True
    , themeMode = Model.ThemeMode.System
    , systemDark = False
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
    , isMac = False
    , networks = []
    , liteNetworks = True
    , liteUnavailable = False
    }
