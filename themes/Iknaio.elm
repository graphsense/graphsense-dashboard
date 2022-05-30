module Iknaio exposing (theme)

import Color exposing (rgb255)
import Css exposing (..)
import Css.Transitions
import Model.Graph exposing (NodeType(..))
import RecordSetter exposing (..)
import Theme.Browser as Browser
import Theme.Button as Button
import Theme.ContextMenu as ContextMenu
import Theme.Dialog as Dialog
import Theme.Graph as Graph
import Theme.Hovercard as Hovercard
import Theme.Search as Search
import Theme.Stats as Stats
import Theme.Table as Table
import Theme.Theme as Theme exposing (Theme, default)
import Theme.User as User
import Util.View exposing (toCssColor)
import VitePluginHelper


type alias Colors =
    { black : Color.Color
    , greyDarkest : Color.Color
    , greyDarker : Color.Color
    , greyDark : Color.Color
    , grey : Color.Color
    , greyLight : Color.Color
    , greyLighter : Color.Color
    , greyLightest : Color.Color
    , white : Color.Color
    , red : Color.Color
    , brandText : Color.Color
    , brandDarker : Color.Color
    , brandDark : Color.Color
    , brandBase : Color.Color
    , brandLight : Color.Color
    , brandLighter : Color.Color
    , brandLightest : Color.Color
    , brandRed : Color.Color
    , brandRedLight : Color.Color
    , brandWhite : Color.Color
    }


colors : Colors
colors =
    { black = rgb255 255 255 255
    , greyDarkest = rgb255 210 213 215
    , greyDarker = rgb255 185 196 204
    , greyDark = rgb255 136 158 174
    , grey = rgb255 88 109 125
    , greyLight = rgb255 45 70 91
    , greyLighter = rgb255 45 70 91
    , greyLightest = rgb255 5 50 84
    , white = rgb255 34 41 47
    , red = rgb255 227 52 47
    , brandText = rgb255 236 243 249
    , brandDarker = rgb255 236 243 249
    , brandDark = rgb255 211 227 241
    , brandBase = rgb255 132 165 194
    , brandLight = rgb255 70 109 145
    , brandLighter = rgb255 7 69 116
    , brandLightest = rgb255 5 50 84
    , brandRed = rgb255 185 86 86
    , brandRedLight = rgb255 241 182 182
    , brandWhite = rgb255 3 31 53
    }


theme : Theme
theme =
    Theme.default
        |> s_scaled scaled
        |> s_logo "/themes/Iknaio/logo.svg"
        |> s_loadingSpinnerUrl "/themes/Iknaio/loading.gif"
        |> s_body
            [ color <| toCssColor colors.brandText
            , fontFamilies [ "system-ui", " BlinkMacSystemFont", " -apple-system", " Segoe UI", " Roboto", " Oxygen", " Ubuntu", " Cantarell", " Fira Sans", " Droid Sans", " Helvetica Neue", " sans-serif" ]
            , scaled 3.5 |> rem |> fontSize
            ]
        |> s_header
            [ backgroundColor <| toCssColor colors.brandWhite
            , scaled 3 |> rem |> padding
            , alignItems center
            ]
        |> s_heading2
            [ fontFamilies [ "Conv_Octarine-Light" ]
            , letterSpacingWide
            , scaled 6 |> rem |> fontSize
            , fontWeight bold
            ]
        |> s_input
            [ outline none
            , inputStyle
            ]
        |> s_headerLogo
            [ maxWidth <| px 190
            ]
        |> s_main
            [ backgroundColor <| toCssColor colors.brandLightest
            ]
        |> s_link
            [ color <| toCssColor colors.brandText
            , hover
                [ textDecoration none
                ]
            ]
        |> s_stats
            (Stats.default
                |> s_root
                    [ scaled statsMargin |> rem |> padding ]
                |> s_stats
                    [ scaled -statsMargin |> rem |> marginLeft ]
                |> s_currency
                    [ backgroundColor <| toCssColor colors.greyLight
                    , scaled statsMargin |> rem |> margin
                    , borderRadiusSm
                    ]
                |> s_currencyHeading
                    [ backgroundColor <| toCssColor colors.brandLight
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
                    , scaled 8 |> rem |> height
                    ]
                |> s_frame
                    [ scaled 1 |> rem |> marginRight
                    , fontFamily monospace
                    ]
                |> s_textarea
                    [ scaled 1 |> rem |> padding
                    , scaled 5 |> rem |> height
                    , inputStyle
                    , scaled 2 |> rem |> paddingX
                    , scaled 2 |> rem |> paddingTop
                    , scaled 1 |> rem |> paddingBottom
                    ]
                |> s_result
                    [ calc (pct 100) minus (scaled 4 |> rem) |> width
                    , scaled 2 |> rem |> padding
                    , borderRadius4
                        zero
                        zero
                        (scaled 1 |> rem)
                        (scaled 1 |> rem)
                    , backgroundColor <| toCssColor colors.brandWhite
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
                    , color <| toCssColor colors.black
                    , display block
                    , scaled 0.5 |> rem |> paddingY
                    , hover
                        [ backgroundColor <| toCssColor colors.brandLighter
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
                        [ backgroundColor <| toCssColor colors.brandLighter
                        ]
                    ]
                |> s_primary
                    [ backgroundColor <| toCssColor colors.greyLight
                    , color <| toCssColor colors.brandDark
                    ]
                |> s_danger
                    [ backgroundColor <| toCssColor colors.brandWhite
                    , color <| toCssColor colors.brandRed
                    ]
                |> s_danger
                    [ backgroundColor <| toCssColor colors.brandWhite
                    , color <| toCssColor colors.brandRed
                    ]
                |> s_disabled
                    [ color <| toCssColor colors.brandLight
                    ]
            )
        |> s_hovercard
            (Hovercard.default
                |> s_borderColor colors.greyLight
                |> s_backgroundColor colors.brandWhite
                |> s_borderWidth 1
                |> s_root
                    [ ( "box-shadow", "0 4px 8px 0 rgba(0, 0, 0, .12), 0 2px 4px 0 rgba(0, 0, 0, .08)" )
                    , ( "border-radius", scaled borderRadiusSmValue |> String.fromFloat |> (\s -> s ++ "rem") )
                    ]
            )
        |> s_user
            (User.default
                |> s_root
                    [ scaled 5 |> rem |> fontSize
                    ]
                |> s_hovercardRoot
                    [ scaled 3 |> rem |> padding
                    ]
            )
        |> s_modal
            (Dialog.default
                |> s_part
                    [ scaled 2 |> rem |> paddingBottom
                    ]
                |> s_heading
                    [ fontWeight bold
                    , scaled 0.1 |> rem |> letterSpacing
                    , scaled 2 |> rem |> paddingBottom
                    , scaled 0.5 |> rem |> paddingTop
                    ]
            )
        |> s_graph
            (Graph.default
                |> s_colorScheme
                    [ rgb255 228 148 68
                    , rgb255 209 97 93
                    , rgb255 133 182 178
                    , rgb255 106 159 88
                    , rgb255 231 202 96
                    , rgb255 168 124 159
                    , rgb255 241 162 169
                    , rgb255 150 118 98
                    , rgb255 184 176 172
                    , rgb255 87 120 164
                    ]
                |> s_lightnessFactor
                    { entity = 1
                    , address = 0.9
                    }
                |> s_saturationFactor
                    { entity = 1
                    , address = 0.9
                    }
                |> s_defaultColor
                    (rgb255 128 128 128)
                |> s_tool
                    [ scaled 2 |> rem |> padding
                    , scaled 4 |> rem |> fontSize
                    , textAlign center
                    , colors.brandBase
                        |> toCssColor
                        |> color
                    , backgroundColor transparent
                    , border (px 0)
                    ]
                |> s_svgRoot
                    [ colors.black
                        |> Color.toCssString
                        |> property "color"
                    , fontWeight (int 300)
                    ]
                |> s_entityCurrency
                    [ px 12 |> fontSize
                    ]
                |> s_entityAddressesCount
                    [ px 14 |> fontSize ]
                |> s_expandHandlePath
                    (\_ isSelected ->
                        let
                            ( color, width ) =
                                if isSelected then
                                    ( colors.red, nodeStrokeWidth * 4 )

                                else
                                    ( colors.white, nodeStrokeWidth )
                        in
                        [ color
                            |> Color.toCssString
                            |> property "stroke"
                        , property "stroke-width" <| String.fromFloat width
                        ]
                    )
                |> s_nodeFrame
                    (\_ isSelected ->
                        let
                            ( color, width ) =
                                if isSelected then
                                    ( colors.red, nodeStrokeWidth * 4 )

                                else
                                    ( colors.white, nodeStrokeWidth )
                        in
                        [ color
                            |> Color.toCssString
                            |> property "stroke"
                        , property "stroke-width" <| String.fromFloat width
                        ]
                    )
                |> s_nodeSeparatorToExpandHandle
                    (\_ ->
                        [ colors.white
                            |> Color.toCssString
                            |> property "stroke"
                        , opacity <| num 0.5
                        , property "stroke-width" "0.5"
                        ]
                    )
                |> s_link
                    (\nodeType hovered ->
                        [ (if hovered then
                            colors.black

                           else if nodeType == Address then
                            colors.grey

                           else
                            colors.greyLighter
                          )
                            |> Color.toCssString
                            |> property "stroke"
                        ]
                    )
                |> s_linkColorFaded colors.grey
                |> s_linkColorStrong colors.black
                |> s_linkColorSelected colors.brandRed
                |> s_linkLabel
                    (\hovered ->
                        [ fontFamily monospace
                        , (if hovered then
                            colors.black

                           else
                            colors.grey
                          )
                            |> Color.toCssString
                            |> property "fill"
                        , cursor pointer
                        ]
                    )
                |> s_linkLabelBox
                    (\_ ->
                        [ Color.toCssString colors.brandLightest
                            |> property "fill"
                        , num 0.8 |> opacity
                        , property "stroke-width" "0"
                        ]
                    )
                |> s_navbar
                    [ toCssColor colors.brandWhite |> backgroundColor
                    ]
            )
        |> s_browser
            (Browser.default
                |> s_propertyBoxTable
                    [ letterSpacingWide
                    ]
                |> s_propertyBoxKey
                    [ fontWeight (int 500)
                    , scaled 2 |> rem |> paddingRight
                    , scaled 1 |> rem |> paddingBottom
                    ]
                |> s_propertyBoxValue
                    [ fontHairline
                    ]
                |> s_frame
                    (\visible ->
                        let
                            p =
                                if visible then
                                    0

                                else
                                    -110
                        in
                        [ Css.Transitions.transition
                            [ Css.Transitions.transform 200
                            ]
                        , displayFlex
                        , translateY (pct p) |> transform
                        , colors.brandWhite
                            |> toCssColor
                            |> backgroundColor
                        , scaled 2 |> rem |> padding
                        , shadowMd
                        ]
                    )
                |> s_propertyBoxRule
                    [ borderWidth (px 0)
                    , scaled 1 |> rem |> margin
                    ]
                |> s_propertyBoxOutgoingTxs
                    [ toCssColor colors.brandRed |> color
                    ]
                |> s_propertyBoxIncomingTxs
                    [ toCssColor colors.brandBase |> color
                    ]
                |> s_propertyBoxTableLink
                    (\isActive ->
                        [ toCssColor
                            (if isActive then
                                colors.brandBase

                             else
                                colors.brandLight
                            )
                            |> color
                        , hover
                            [ toCssColor colors.brandBase |> color
                            ]
                        , active
                            [ toCssColor colors.brandBase |> color
                            ]
                        ]
                    )
            )
        |> s_table
            (Table.default
                |> s_root
                    [ px 10 |> paddingX
                    ]
                |> s_table
                    [ scaled 1 |> rem |> padding
                    ]
                |> s_headCell
                    [ tableCell
                    ]
                |> s_headRow
                    [ textAlign left
                    , fontWeight bold
                    ]
                |> s_headCellSortable
                    [ ( "cursor", "pointer" )
                    ]
                |> s_row
                    [ nthChild "2n"
                        [ colors.brandLightest |> toCssColor |> backgroundColor
                        ]
                    ]
                |> s_cell
                    [ tableCell
                    ]
                |> s_numberCell
                    [ numberCell
                    ]
                |> s_valuesCell
                    (\isNegative ->
                        numberCell
                            :: (if isNegative then
                                    [ toCssColor colors.brandRed |> color ]

                                else
                                    []
                               )
                    )
            )
        |> s_contextMenu
            (ContextMenu.default
                |> s_root
                    [ colors.brandWhite |> toCssColor |> backgroundColor
                    , shadowMd
                    ]
                |> s_option
                    [ scaled 2 |> rem |> padding
                    , hover
                        [ colors.brandBase |> toCssColor |> backgroundColor
                        ]
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


statsMargin : Float
statsMargin =
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


borderRadiusSmValue : Float
borderRadiusSmValue =
    0.5


borderRadiusSm : Style
borderRadiusSm =
    scaled borderRadiusSmValue |> rem |> borderRadius


inputStyle : Style
inputStyle =
    batch
        [ backgroundColor <| toCssColor colors.greyLight
        , color <| toCssColor colors.black
        , borderRadiusSm
        , border zero
        , scaled 0.5 |> rem |> padding
        ]


spinnerHeight : Float
spinnerHeight =
    4


spinnerPadding : Float
spinnerPadding =
    1.5


letterSpacingWide : Style
letterSpacingWide =
    scaled 0.2 |> rem |> letterSpacing


entityStrokeDashArray : String
entityStrokeDashArray =
    "4 1"


nodeStrokeWidth : Float
nodeStrokeWidth =
    0.5


shadowMd : Style
shadowMd =
    property "box-shadow" "0 4px 8px 0 rgba(0, 0, 0, .12), 0 2px 4px 0 rgba(0, 0, 0, .08)"


tableCell : Style
tableCell =
    [ scaled 1 |> rem |> padding
    , whiteSpace noWrap
    ]
        |> batch


numberCell : Style
numberCell =
    [ tableCell
    , textAlign right
    ]
        |> batch
