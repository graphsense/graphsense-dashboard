module Iknaio exposing (theme)

import Css exposing (..)
import RecordSetter exposing (..)
import Theme.Stats as Stats
import Theme.Theme as Theme exposing (Theme, default)
import VitePluginHelper


type alias Colors =
    { black : Color
    , greyDarkest : Color
    , greyDarker : Color
    , greyDark : Color
    , grey : Color
    , greyLight : Color
    , greyLighter : Color
    , greyLightest : Color
    , white : Color
    , brandText : Color
    , brandDarker : Color
    , brandDark : Color
    , brandBase : Color
    , brandLight : Color
    , brandLighter : Color
    , brandLightest : Color
    , brandRed : Color
    , brandRedLight : Color
    , brandWhite : Color
    }


colors : Colors
colors =
    { black = hex "fff"
    , greyDarkest = hex "d2d5d7"
    , greyDarker = hex "b9c4cc"
    , greyDark = hex "889eae"
    , grey = hex "586d7d"
    , greyLight = hex "2d465b"
    , greyLighter = hex "2d465b"
    , greyLightest = hex "053254"
    , white = hex "22292F"
    , brandText = hex "ecf3f9"
    , brandDarker = hex "ecf3f9"
    , brandDark = hex "d3e3f1"
    , brandBase = hex "84a5c2"
    , brandLight = hex "466d91"
    , brandLighter = hex "074574"
    , brandLightest = hex "053254"
    , brandRed = hex "b95656"
    , brandRedLight = hex "f1b6b6"
    , brandWhite = hex "031f35"
    }


theme : Theme
theme =
    Theme.default
        |> s_scaled scaled
        |> s_logo "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/logo.svg]"
        |> s_body
            [ color colors.brandText
            , fontFamilies [ "Roboto", "sans-serif" ]
            , scaled 3.5 |> rem |> fontSize
            ]
        |> s_header
            [ backgroundColor colors.white
            , scaled 3 |> rem |> padding
            ]
        |> s_heading2
            [ fontFamilies [ "Conv_Octarine-Light" ]
            , scaled 0.2 |> rem |> letterSpacing
            , scaled 6 |> rem |> fontSize
            , fontWeight bold
            ]
        |> s_headerLogo
            [ maxWidth <| px 190
            ]
        |> s_main
            [ backgroundColor colors.brandLightest
            , scaled mainMargin |> rem |> padding
            ]
        |> s_stats
            (Stats.default
                |> s_root
                    [ scaled -mainMargin |> rem |> marginLeft
                    ]
                |> s_currency
                    [ backgroundColor colors.greyLight
                    , scaled mainMargin |> rem |> margin
                    ]
                |> s_currencyHeading
                    [ backgroundColor colors.brandLight
                    , fontHairline
                    , scaled 2 |> rem |> padding
                    , scaled 5 |> rem |> fontSize
                    , scaled 3.5 |> rem |> paddingTop
                    , scaled currencyPadding |> rem |> paddingLeft
                    , width auto
                    , fontFamilies [ "Conv_Octarine-Light" ]
                    ]
                |> s_tableWrapperInner
                    [ currencyPadding / 2 |> scaled |> rem |> padding
                    ]
                |> s_table
                    [ scaled 0.1 |> rem |> letterSpacing
                    , currencyPadding / 2 |> scaled |> rem |> borderSpacing
                    ]
                |> s_tableCellKey
                    [ int 500 |> fontWeight
                    , scaled 2 |> rem |> paddingRight
                    ]
                |> s_currencyBackground
                    [ rgba 0 0 0 0.2 |> color
                    ]
            )
        |> s_custom
            -- need to put these special references in separate string expressions to make the vite resolution work
            ("[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/fonts/Octarine-Light/fonts.css]"
                ++ "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/fonts/Octarine-Bold/fonts.css]"
            )


scaled : Float -> Float
scaled =
    (*) 0.25


fontHairline : Style
fontHairline =
    fontWeight (int 100)


currencyPadding : Float
currencyPadding =
    4


mainMargin : Float
mainMargin =
    5
