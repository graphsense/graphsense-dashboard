module Iknaio exposing (theme)

import Css exposing (..)
import RecordSetter exposing (..)
import Theme.Button as Button
import Theme.Search as Search
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
        |> s_logo "/themes/Iknaio/logo.svg"
        |> s_loadingSpinnerUrl "/themes/Iknaio/loading.gif"
        |> s_body
            [ color colors.brandText
            , fontFamilies [ "Roboto", "sans-serif" ]
            , scaled 3.5 |> rem |> fontSize
            ]
        |> s_header
            [ backgroundColor colors.brandWhite
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
                    , borderRadiusSm
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
                |> s_tableCellValue
                    [ fontWeight (int 300)
                    ]
                |> s_currencyBackground
                    [ rgba 0 0 0 0.2 |> color
                    ]
            )
        |> s_search
            (Search.default
                |> s_form
                    [ scaled 3 |> rem |> fontSize
                    ]
                |> s_frame
                    [ scaled 1 |> rem |> marginRight
                    , fontFamily monospace
                    ]
                |> s_textarea
                    [ scaled 1 |> rem |> padding
                    , outline none
                    , inputStyle
                    , scaled 5 |> rem |> height
                    ]
                |> s_result
                    [ calc (pct 100) minus (scaled 4 |> rem) |> width
                    , scaled 2 |> rem |> padding
                    , borderRadius4
                        zero
                        zero
                        (scaled 1 |> rem)
                        (scaled 1 |> rem)
                    , backgroundColor colors.brandWhite
                    , spinnerHeight |> scaled |> rem |> minHeight
                    ]
                |> s_loadingSpinner
                    [ top zero
                    , right zero
                    , scaled spinnerHeight |> rem |> height
                    , scaled spinnerPadding |> rem |> padding
                    ]
                |> s_resultGroupTitle
                    [ fontWeight bold
                    , paddingY (scaled 1 |> rem)
                    ]
                |> s_resultLine
                    [ textDecoration none
                    , color colors.black
                    , display block
                    , scaled 0.5 |> rem |> paddingY
                    , hover
                        [ backgroundColor colors.brandLighter
                        ]
                    ]
                |> s_resultLineIcon
                    [ opacity <| num 0.5
                    , scaled 1 |> rem |> paddingRight
                    ]
            )
        |> s_button
            (Button.default
                |> s_base
                    [ fontWeight bold
                    , scaled 1 |> rem |> paddingY
                    , scaled 2 |> rem |> paddingX
                    , borderRadiusSm
                    , calc (pct 100) minus (px 1) |> height
                    , border zero
                    , hover
                        [ backgroundColor colors.brandLighter
                        ]
                    ]
                |> s_primary
                    [ backgroundColor colors.greyLight
                    , color colors.brandDark
                    ]
                |> s_danger
                    [ backgroundColor colors.brandWhite
                    , color colors.brandRed
                    ]
                |> s_danger
                    [ backgroundColor colors.brandWhite
                    , color colors.brandRed
                    ]
                |> s_disabled
                    [ color colors.brandLight
                    ]
            )
        |> s_custom
            -- need to put these special references in separate string expressions to make the vite resolution work
            ("[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/fonts/Octarine-Light/fonts.css]"
                ++ " ::placeholder { color: inherit; opacity: 0.5 }"
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


wFull : Style
wFull =
    width <| pct 100


paddingY : Length compatibleB unitsB -> Style
paddingY y =
    batch
        [ paddingTop y
        , paddingBottom y
        ]


paddingX : Length compatibleB unitsB -> Style
paddingX x =
    batch
        [ paddingLeft x
        , paddingRight x
        ]


borderRadiusSm : Style
borderRadiusSm =
    scaled 0.5 |> rem |> borderRadius


inputStyle : Style
inputStyle =
    batch
        [ backgroundColor colors.greyLight
        , scaled 2 |> rem |> paddingX
        , scaled 2 |> rem |> paddingTop
        , scaled 1 |> rem |> paddingBottom
        , color colors.black
        , borderRadiusSm
        , border zero
        ]


spinnerHeight : Float
spinnerHeight =
    4


spinnerPadding : Float
spinnerPadding =
    1.5
