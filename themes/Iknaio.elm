module Iknaio exposing (theme)

import Css exposing (..)
import Theme exposing (Colors, Theme)
import VitePluginHelper


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
    { scaled = scaled
    , logo = "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/logo.svg]"
    , body =
        batch
            [ color colors.brandText
            , fontFamilies [ "Roboto", "sans-serif" ]
            , scaled 3.5 |> rem |> fontSize
            ]
    , header =
        batch
            [ backgroundColor colors.white
            , scaled 3 |> rem |> padding
            ]
    , heading2 =
        batch
            [ fontFamilies [ "Conv_Octarine-Light" ]
            , scaled 0.2 |> rem |> letterSpacing
            , scaled 6 |> rem |> fontSize
            , fontWeight bold
            ]
    , headerLogo =
        batch
            [ maxWidth <| px 190
            ]
    , addonsNav = batch []
    , main =
        batch
            [ backgroundColor colors.brandLightest
            , scaled mainMargin |> rem |> padding
            ]
    , stats =
        { root =
            batch
                [ scaled -mainMargin |> rem |> marginLeft
                ]
        , currency =
            batch
                [ backgroundColor colors.greyLight
                , scaled mainMargin |> rem |> margin
                ]
        , currencyHeading =
            batch
                [ backgroundColor colors.brandLight
                , fontHairline
                , scaled 2 |> rem |> padding
                , scaled 5 |> rem |> fontSize
                , scaled 3.5 |> rem |> paddingTop
                , scaled currencyPadding |> rem |> paddingLeft
                , width auto
                , fontFamilies [ "Conv_Octarine-Light" ]
                ]
        , tableWrapper =
            batch []
        , tableWrapperInner =
            batch
                [ currencyPadding / 2 |> scaled |> rem |> padding
                ]
        , table =
            batch
                [ scaled 0.2 |> rem |> letterSpacing
                , currencyPadding / 2 |> scaled |> rem |> borderSpacing
                ]
        , tableRow = batch []
        , tableCellKey =
            batch
                [ int 500 |> fontWeight
                , scaled 2 |> rem |> paddingRight
                ]
        , tableCellValue = batch []
        , currencyBackground =
            batch
                [ rgba 0 0 0 0.2 |> color
                ]
        , currencyBackgroundPath = batch []
        }
    , custom =
        -- need to put these special references in separate string expressions to make the vite resolution work
        "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/fonts/Octarine-Light/fonts.css]"
            ++ "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/fonts/Octarine-Bold/fonts.css]"
    }


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
