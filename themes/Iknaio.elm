module Iknaio exposing (theme)

import Color exposing (rgb255)
import Css exposing (..)
import Css.Transitions
import Model.Graph exposing (NodeType(..))
import Model.Graph.Tool as Tool
import RecordSetter exposing (..)
import Theme.Autocomplete as Autocomplete
import Theme.Browser as Browser
import Theme.Button as Button
import Theme.ContextMenu as ContextMenu
import Theme.Dialog as Dialog
import Theme.Graph as Graph
import Theme.Hovercard as Hovercard
import Theme.Search as Search
import Theme.Stats as Stats
import Theme.Statusbar as Statusbar
import Theme.Table as Table
import Theme.Theme as Theme exposing (Theme, default)
import Theme.User as User
import Tuple exposing (..)
import Util.Theme
    exposing
        ( backgroundColorWithLightmode
        , borderColorWithLightmode
        , borderColor_backgroundColorWithLightmode
        , colorWithLightmode
        , color_backgroundColorWithLightmode
        , switchColor
        )
import Util.View exposing (toCssColor)
import VitePluginHelper


type alias Colors =
    { black : Theme.SwitchableColor
    , greyDarkest : Theme.SwitchableColor
    , greyDarker : Theme.SwitchableColor
    , greyDark : Theme.SwitchableColor
    , grey : Theme.SwitchableColor
    , greyLight : Theme.SwitchableColor
    , greyLighter : Theme.SwitchableColor
    , greyLightest : Theme.SwitchableColor
    , white : Theme.SwitchableColor
    , red : Theme.SwitchableColor
    , brandText : Theme.SwitchableColor
    , brandDarker : Theme.SwitchableColor
    , brandDark : Theme.SwitchableColor
    , brandBase : Theme.SwitchableColor
    , brandLight : Theme.SwitchableColor
    , brandLighter : Theme.SwitchableColor
    , brandLightest : Theme.SwitchableColor
    , brandRed : Theme.SwitchableColor
    , brandRedLight : Theme.SwitchableColor
    , brandWhite : Theme.SwitchableColor
    }


colors : Colors
colors =
    { black = { dark = rgb255 255 255 255, light = rgb255 0 0 0 }
    , greyDarkest = { dark = rgb255 210 213 215, light = rgb255 61 72 82 }
    , greyDarker = { dark = rgb255 185 196 204, light = rgb255 96 111 123 }
    , greyDark = { dark = rgb255 136 158 174, light = rgb255 113 127 133 }
    , grey = { dark = rgb255 88 109 125, light = rgb255 199 209 210 }
    , greyLight = { dark = rgb255 45 70 91, light = rgb255 241 245 248 }
    , greyLighter = { dark = rgb255 45 70 91, light = rgb255 241 245 248 }
    , greyLightest = { dark = rgb255 5 50 84, light = rgb255 248 250 252 }
    , white = { dark = rgb255 34 41 47, light = rgb255 255 255 255 }
    , red = { dark = rgb255 227 52 47, light = rgb255 227 52 47 }
    , brandText = { dark = rgb255 236 243 249, light = rgb255 51 51 51 }
    , brandDarker = { dark = rgb255 236 243 249, light = rgb255 113 133 138 }
    , brandDark = { dark = rgb255 211 227 241, light = rgb255 113 161 165 }
    , brandBase = { dark = rgb255 132 165 194, light = rgb255 107 194 194 }
    , brandLight = { dark = rgb255 70 109 145, light = rgb255 192 226 225 }
    , brandLighter = { dark = rgb255 7 69 116, light = rgb255 210 236 237 }
    , brandLightest = { dark = rgb255 5 50 84, light = rgb255 248 250 252 }
    , brandRed = { dark = rgb255 185 86 86, light = rgb255 204 106 66 }
    , brandRedLight = { dark = rgb255 241 182 182, light = rgb255 238 204 190 }
    , brandWhite = { dark = rgb255 3 31 53, light = rgb255 255 255 255 }
    }


fontFam =
    [ "system-ui", " BlinkMacSystemFont", " -apple-system", " Segoe UI", " Roboto", " Oxygen", " Ubuntu", " Cantarell", " Fira Sans", " Droid Sans", " Helvetica Neue", " sans-serif" ]


theme : Theme
theme =
    Theme.default
        |> s_scaled scaled
        |> s_loadingSpinnerUrl "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/loading.gif]"
        |> s_body
            (\lightmode ->
                [ colorWithLightmode lightmode colors.brandText
                , fontFamilies fontFam
                , scaled 3.5 |> rem |> fontSize
                ]
            )
        |> s_header
            (\lightmode ->
                [ backgroundColorWithLightmode lightmode colors.brandWhite
                , scaled 3 |> rem |> padding
                , alignItems center
                , shadowSm
                ]
            )
        |> s_headerTitle
            [ fontFamilies [ "Conv_Octarine-Light" ]
            , scaled 5 |> rem |> fontSize
            , fontWeight bold
            , letterSpacingWide
            , display inline
            , scaled 2 |> rem |> marginLeft
            ]
        |> s_heading2
            [ fontFamilies [ "Conv_Octarine-Light" ]
            , letterSpacingWide
            , scaled 6 |> rem |> fontSize
            , fontWeight bold
            , scaled 1 |> rem |> paddingTop
            ]
        |> s_paragraph
            [ scaled 2 |> rem |> marginBottom
            ]
        |> s_listItem
            [ listStyleType disc
            , scaled 6 |> rem |> marginLeft
            ]
        |> s_inputRaw (\lightmode -> inputStyleRaw lightmode)
        |> s_headerLogo
            [ maxWidth <| px 190
            ]
        |> s_headerLogoWrap
            []
        |> s_sidebar
            (\lightmode ->
                [ backgroundColorWithLightmode lightmode colors.brandWhite
                , shadowSm
                ]
            )
        |> s_sidebarIcon
            (\lightmode active ->
                [ colorWithLightmode lightmode iconInactive
                , scaled 5 |> rem |> fontSize
                , scaled 4 |> rem |> padding
                ]
                    ++ (if active then
                            [ color_backgroundColorWithLightmode lightmode iconActive colors.brandLightest
                            ]

                        else
                            [ hover
                                [ switchColor lightmode iconHovered |> toCssColor |> color
                                ]
                            ]
                       )
            )
        |> s_main
            (\lightmode ->
                [ backgroundColorWithLightmode lightmode colors.brandLightest
                ]
            )
        |> s_navbar
            (\lightmode ->
                [ backgroundColorWithLightmode lightmode colors.brandWhite
                , shadowSm
                ]
            )
        |> s_link
            (\lightmode ->
                [ colorWithLightmode lightmode colors.brandText
                , hover
                    [ textDecoration none
                    ]
                ]
            )
        |> s_iconLink
            (\lightmode ->
                [ colorWithLightmode lightmode colors.brandText
                , hover
                    [ textDecoration none
                    ]
                , scaled 5 |> rem |> fontSize
                , scaled 1 |> rem |> padding
                ]
            )
        |> s_loadingSpinner [ loadingSpinner ]
        |> s_logo "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/logo.svg]"
        |> s_popup
            (\lightmode ->
                [ scaled 5 |> rem |> padding
                , borderColor_backgroundColorWithLightmode lightmode colors.greyLight colors.brandWhite
                , borderWidth <| px 1
                , borderRadius <| px 5
                ]
            )
        |> s_logo_lightmode "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/logo_light.svg]"
        |> s_overlay
            [ Color.rgba 0 0 0 0.6 |> toCssColor |> backgroundColor
            ]
        |> s_switchLabel
            [ whiteSpace noWrap
            ]
        |> s_switchRoot
            [ displayFlex
            , alignItems center
            ]
        |> s_userDefautImgUrl "[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/circle-question-regular.svg]"
        |> s_switchOnColor
            (\lightmode ->
                if lightmode then
                    colors.brandLight.light

                else
                    colors.brandLight.dark
            )
        |> s_disabled
            (\lightmode ->
                let
                    c =
                        (if lightmode then
                            iconDisabled.light

                         else
                            iconDisabled.dark
                        )
                            |> toCssColor
                            |> color
                in
                [ c
                , hover
                    [ c ]
                ]
            )
        |> s_stats
            (Stats.default
                |> s_root
                    [ scaled statsMargin |> rem |> padding ]
                |> s_stats
                    [ scaled -statsMargin |> rem |> marginLeft ]
                |> s_currency
                    (\lightmode ->
                        [ backgroundColorWithLightmode lightmode colors.greyLight
                        , scaled statsMargin |> rem |> margin
                        , borderRadiusSm
                        ]
                    )
                |> s_currencyHeading
                    (\lightmode ->
                        [ backgroundColorWithLightmode lightmode colors.brandLight
                        , fontHairline
                        , scaled 2 |> rem |> padding
                        , scaled 5 |> rem |> fontSize
                        , scaled 3.5 |> rem |> paddingTop
                        , scaled currencyPadding |> rem |> paddingLeft
                        , width auto
                        , fontFamilies [ "Conv_Octarine-Light" ]
                        ]
                    )
                |> s_tableWrapperInner
                    [ currencyPadding / 2 |> scaled |> rem |> padding
                    ]
                |> s_table
                    [ scaled 0.1 |> rem |> letterSpacing
                    , currencyPadding / 2 |> scaled |> rem |> borderSpacing
                    ]
                |> s_tableCellKey
                    [ fontBold
                    , scaled 2 |> rem |> paddingRight
                    ]
                |> s_tableCellValue
                    [ fontNormal
                    ]
                |> s_currencyBackground
                    [ rgba 0 0 0 0.2 |> color
                    ]
                |> s_loadingSpinner
                    [ scaled 8 |> rem |> height
                    , scaled 8 |> rem |> width
                    , scaled 2 |> rem |> paddingTop
                    , scaled -1 |> rem |> marginLeft
                    ]
            )
        |> s_search
            (Search.default
                |> s_frame
                    [ scaled 1 |> rem |> marginRight
                    , fontFamily monospace
                    ]
                |> s_form
                    [ scaled 3 |> rem |> fontSize
                    , scaled 8 |> rem |> height
                    ]
                |> s_textarea
                    (\lightmode input ->
                        [ scaled 1 |> rem |> padding
                        , scaled 5 |> rem |> height
                        , inputStyle lightmode
                        , scaled 2 |> rem |> paddingX
                        , scaled 2 |> rem |> paddingTop
                        , scaled 1 |> rem |> paddingBottom
                        ]
                            ++ (if String.isEmpty input then
                                    [ fontFamilies fontFam
                                    , scaled 3.5 |> rem |> fontSize
                                    ]

                                else
                                    []
                               )
                    )
                |> s_resultGroupTitle
                    [ fontWeight bold
                    , paddingY (scaled 1 |> rem)
                    ]
                |> s_resultLine
                    (\lightmode ->
                        [ textDecoration none
                        , colorWithLightmode lightmode colors.black
                        , display block
                        , scaled 0.5 |> rem |> paddingY
                        , hover
                            [ backgroundColorWithLightmode lightmode colors.brandLighter
                            ]
                        ]
                    )
                |> s_resultLineIcon
                    [ opacity <| num 0.5
                    , scaled 1 |> rem |> paddingRight
                    ]
                |> s_button
                    []
            )
        |> s_autocomplete
            (Autocomplete.default
                |> s_result
                    (\lightmode ->
                        [ calc (pct 100) minus (scaled 4 |> rem) |> width
                        , scaled 2 |> rem |> padding
                        , borderRadius4
                            zero
                            zero
                            (scaled 1 |> rem)
                            (scaled 1 |> rem)
                        , backgroundColorWithLightmode lightmode colors.brandWhite
                        , spinnerHeight |> scaled |> rem |> minHeight
                        , scaled 3.5 |> rem |> fontSize
                        , shadowMd
                        ]
                    )
                |> s_loadingSpinner
                    [ position absolute
                    , top zero
                    , right zero
                    , loadingSpinner
                    ]
            )
        |> s_button
            (Button.default
                |> s_button
                    (\lightmode ->
                        [ fontWeight bold
                        , scaled 1 |> rem |> paddingY
                        , scaled 2 |> rem |> paddingX
                        , scaled 1 |> rem |> marginX
                        , borderRadiusSm
                        , border zero
                        , hover
                            [ backgroundColorWithLightmode lightmode colors.brandLighter
                            ]
                        ]
                    )
                |> s_primary
                    (\lightmode ->
                        [ color_backgroundColorWithLightmode lightmode colors.brandDark colors.greyLighter
                        ]
                    )
                |> s_danger
                    (\lightmode ->
                        [ color_backgroundColorWithLightmode lightmode colors.brandRed colors.brandWhite
                        ]
                    )
                |> s_disabled
                    (\lightmode ->
                        [ colorWithLightmode lightmode colors.brandLight
                        ]
                    )
            )
        |> s_hovercard
            (\lightmode ->
                Hovercard.default
                    |> s_borderColor (switchColor lightmode colors.greyLight)
                    |> s_backgroundColor (switchColor lightmode colors.brandWhite)
                    |> s_borderWidth 1
                    |> s_root
                        [ ( "box-shadow", "0 4px 8px 0 rgba(0, 0, 0, .12), 0 2px 4px 0 rgba(0, 0, 0, .08)" )
                        , ( "border-radius", scaled borderRadiusSmValue |> String.fromFloat |> (\s -> s ++ "rem") )
                        ]
            )
        |> s_user
            (User.default
                |> s_root
                    (\lightmode ->
                        [ scaled 5 |> rem |> fontSize
                        , colorWithLightmode lightmode iconActive
                        ]
                    )
                |> s_hovercardRoot
                    [ scaled 3 |> rem |> padding
                    ]
                |> s_logoutButton
                    (\lightmode ->
                        [ padding zero
                        , backgroundColor transparent
                        , border zero
                        , colors.brandText |> colorWithLightmode lightmode
                        , textDecoration underline
                        , cursor pointer
                        , hover
                            [ textDecoration none
                            ]
                        ]
                    )
            )
        |> s_dialog
            (Dialog.default
                |> s_dialog
                    (\lightmode ->
                        [ borderColor_backgroundColorWithLightmode lightmode colors.brandLight colors.brandWhite
                        , scaled 3 |> rem |> padding
                        , borderRadiusSm
                        , borderWidth (px 1)
                        , borderStyle solid
                        ]
                    )
                |> s_buttons
                    [ displayFlex
                    , justifyContent spaceBetween
                    , margin2 zero auto
                    , px 100 |> minWidth
                    , pct 50 |> width
                    ]
                |> s_part
                    [ scaled 2 |> rem |> paddingBottom
                    , scaled 2 |> rem |> paddingRight
                    ]
                |> s_heading
                    [ fontBold
                    , scaled 0.1 |> rem |> letterSpacing
                    , scaled 2 |> rem |> paddingBottom
                    , scaled 0.5 |> rem |> paddingTop
                    , whiteSpace noWrap
                    ]
                |> s_headRow
                    (\lightmode ->
                        [ scaled 3 |> rem |> padding
                        , backgroundColorWithLightmode lightmode colors.brandLight
                        , displayFlex
                        , justifyContent spaceBetween
                        , alignItems center
                        , whiteSpace noWrap
                        ]
                    )
                |> s_body
                    [ scaled 3 |> rem |> padding
                    ]
                |> s_headRowClose
                    (\lightmode ->
                        [ colors.brandText |> colorWithLightmode lightmode
                        , backgroundColor transparent
                        , border (px 0)

                        --, position absolute
                        --, scaled 3 |> rem |> top
                        --, scaled 1 |> rem |> right
                        , px 15 |> width
                        , px 20 |> height
                        , cursor pointer
                        ]
                    )
            )
        |> s_graph
            (Graph.default
                |> s_contextMenuRule
                    [ borderWidth (px 0)
                    , scaled 1 |> rem |> margin
                    ]
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
                |> (\graph ->
                        s_highlightsColorScheme
                            (graph.colorScheme
                                |> List.map
                                    (Color.toHsla
                                        >> (\c -> { c | saturation = 1 })
                                        >> Color.fromHsla
                                    )
                            )
                            graph
                   )
                |> s_lightnessFactor
                    (\lightmode ->
                        { entity =
                            if lightmode then
                                1.2

                            else
                                1
                        , address =
                            if lightmode then
                                1.1

                            else
                                0.9
                        }
                    )
                |> s_saturationFactor
                    (\lightmode ->
                        { entity =
                            if lightmode then
                                1.2

                            else
                                1
                        , address =
                            if lightmode then
                                1.1

                            else
                                0.9
                        }
                    )
                |> s_defaultColor
                    (rgb255 138 138 138)
                |> s_tool
                    (\lightmode status ->
                        [ scaled 2 |> rem |> padding
                        , scaled 4 |> rem |> fontSize
                        , textAlign center
                        , colorWithLightmode lightmode <|
                            case status of
                                Tool.Active ->
                                    iconActive

                                Tool.Disabled ->
                                    iconDisabled

                                Tool.Inactive ->
                                    iconInactive
                        , transparent
                            |> backgroundColor
                        , border (px 0)
                        , hover
                            (if status /= Tool.Disabled then
                                [ switchColor lightmode iconHovered |> toCssColor |> color
                                ]

                             else
                                []
                            )
                        ]
                    )
                |> s_svgRoot
                    (\lightmode ->
                        [ Util.Theme.switchColor lightmode colors.black
                            |> Color.toCssString
                            |> property "color"
                        , fontWeight (int 300)
                        ]
                    )
                |> s_expandHandleText
                    (\lightmode _ -> fillBlack lightmode)
                |> s_addressLabel
                    (\lightmode -> fillBlack lightmode)
                |> s_entityLabel
                    (\lightmode -> fillBlack lightmode)
                |> s_abuseFlag
                    (\lightmode ->
                        [ colors.red
                            |> switchColor lightmode
                            |> Color.toCssString
                            |> property "fill"
                        , colors.white
                            |> switchColor lightmode
                            |> Color.toCssString
                            |> property "stroke"
                        , property "stroke-width" "10px"
                        ]
                    )
                |> s_flag
                    (\lightmode ->
                        [ colors.white
                            |> switchColor lightmode
                            |> Color.toCssString
                            |> property "stroke"
                        , Util.Theme.switchColor lightmode colors.black
                            |> Color.toCssString
                            |> property "fill"
                        , property "stroke-width" "10px"
                        ]
                    )
                |> s_entityCurrency
                    (\lightmode ->
                        (px 12 |> fontSize)
                            :: property "dominant-baseline" "hanging"
                            :: fillBlack lightmode
                    )
                |> s_entityAddressesCount
                    (\lightmode ->
                        (px 14 |> fontSize) :: fillBlack lightmode
                    )
                |> s_expandHandlePath
                    (\lightmode _ isSelected ->
                        let
                            ( color, width ) =
                                if isSelected then
                                    ( switchColor lightmode colors.red, nodeStrokeWidth * 4 )

                                else
                                    ( switchColor lightmode colors.black, nodeStrokeWidth )
                        in
                        [ color
                            |> Color.toCssString
                            |> property "stroke"
                        , property "stroke-width" <| String.fromFloat width
                        ]
                    )
                |> s_nodeFrame
                    (\lightmode _ isSelected ->
                        let
                            ( color, width ) =
                                if isSelected then
                                    ( switchColor lightmode colors.red, nodeStrokeWidth * 4 )

                                else
                                    ( switchColor lightmode colors.black, nodeStrokeWidth )
                        in
                        [ color
                            |> Color.toCssString
                            |> property "stroke"
                        , property "stroke-width" <| String.fromFloat <| width * 0.8
                        ]
                    )
                |> s_nodeSeparatorToExpandHandle
                    (\lightmode _ ->
                        [ switchColor lightmode colors.black
                            |> Color.toCssString
                            |> property "stroke"
                        , opacity <| num 0.5
                        , property "stroke-width" "0.5"
                        ]
                    )
                |> s_link
                    (\lightmode nodeType hovered selected highlight ->
                        [ (if hovered then
                            Util.Theme.switchColor lightmode colors.black

                           else if selected then
                            switchColor lightmode colors.red

                           else
                            highlight
                                |> Maybe.withDefault
                                    (if nodeType == AddressType then
                                        switchColor lightmode colors.grey

                                     else
                                        switchColor lightmode colors.grey
                                    )
                          )
                            |> Color.toCssString
                            |> property "stroke"
                        ]
                    )
                |> s_linkColorFaded (\lightmode -> Util.Theme.switchColor lightmode colors.grey)
                |> s_linkColorStrong (\lightmode -> Util.Theme.switchColor lightmode colors.black)
                |> s_linkColorSelected (\lightmode -> switchColor lightmode colors.red)
                |> s_linkLabel
                    (\lightmode hovered selected color ->
                        [ fontFamily monospace
                        , (if hovered then
                            Util.Theme.switchColor lightmode colors.black

                           else if selected then
                            switchColor lightmode colors.red

                           else
                            color
                                |> Maybe.withDefault
                                    (Util.Theme.switchColor lightmode colors.grey)
                          )
                            |> Color.toCssString
                            |> property "fill"
                        , cursor pointer
                        ]
                    )
                |> s_linkLabelBox
                    (\lightmode _ _ ->
                        [ switchColor lightmode colors.brandLightest
                            |> Color.toCssString
                            |> property "fill"
                        , num 0.8 |> opacity
                        , property "stroke-width" "0"
                        ]
                    )
                |> s_shadowLink
                    (\lightmode ->
                        [ switchColor lightmode colors.brandLighter
                            |> Color.toCssString
                            |> property "fill"
                        , property "fill-opacity" "0.5"
                        ]
                    )
                |> s_searchTextarea
                    (\lightmode ->
                        [ inputStyle lightmode
                        , scaled 1 |> rem |> padding
                        , fontFamilies fontFam
                        ]
                    )
                |> s_toolbox
                    (\lightmode visible ->
                        let
                            p =
                                if visible then
                                    0

                                else
                                    -110
                        in
                        [ backgroundColorWithLightmode lightmode colors.brandWhite
                        , scaled 2 |> rem |> padding
                        , Css.Transitions.transition
                            [ Css.Transitions.transform 200
                            ]
                        , translateY (pct p) |> transform
                        , shadowSm
                        ]
                    )
                |> s_legendItem
                    [ displayFlex
                    , alignItems center
                    , scaled 1 |> rem |> marginTop
                    , whiteSpace noWrap
                    ]
                |> s_legendItemColor
                    [ before
                        -- seems not to work
                        [ property "content" "◼"
                        ]
                    , scaled 1 |> rem |> marginRight
                    ]
                |> s_radio
                    []
                |> s_radioText
                    [ scaled 1 |> rem |> padding
                    ]
                |> s_searchSettingsRow
                    [ displayFlex
                    , justifyContent spaceBetween
                    ]
                |> s_tagLockedIcon
                    [ opacity (num 0.7) ]
                |> s_tagLockedText
                    [ fontStyle italic ]
                |> s_highlightsColors
                    [ scaled 2 |> rem |> marginBottom
                    , displayFlex
                    ]
                |> s_highlightsColor
                    [ scaled 5 |> rem |> fontSize
                    , scaled 1 |> rem |> paddingRight
                    , cursor pointer
                    ]
                |> s_highlightRoot
                    [ displayFlex
                    , alignItems center
                    ]
                |> s_highlightColor
                    (\lightmode selected ->
                        [ scaled 5 |> rem |> fontSize
                        , scaled 1 |> rem |> marginRight
                        , borderBottomWidth <| px 2
                        , borderStyle solid
                        ]
                            ++ (if selected then
                                    [ borderColorWithLightmode lightmode colors.brandDark
                                    ]

                                else
                                    [ borderColorWithLightmode lightmode colors.brandWhite
                                    ]
                               )
                    )
                |> s_highlightTitle
                    (\lightmode ->
                        [ inputStyle lightmode
                        , scaled 3 |> rem |> fontSize
                        ]
                    )
                |> s_highlightTrash
                    (\lightmode ->
                        [ colorWithLightmode lightmode iconInactive
                        , hover
                            [ switchColor lightmode iconHovered
                                |> toCssColor
                                |> color
                            ]
                        , scaled 1 |> rem |> paddingLeft
                        , cursor pointer
                        ]
                    )
            )
        |> s_browser
            (Browser.default
                |> s_propertyBoxTable
                    [ letterSpacingWide
                    ]
                |> s_propertyBoxNote
                    (\lightmode ->
                        [ scaled 1 |> rem |> paddingLeft
                        ]
                    )
                |> s_propertyBoxRow
                    (\lightmode ->
                        [ hover
                            [ backgroundColorWithLightmode lightmode colors.brandLightest
                            ]
                        ]
                    )
                |> s_propertyBoxKey
                    [ fontBold
                    , scaled 2 |> rem |> paddingRight
                    , scaled 0.5 |> rem |> paddingY
                    , whiteSpace noWrap
                    ]
                |> s_propertyBoxValue
                    [ fontNormal
                    ]
                |> s_propertyBoxValueInner
                    [ displayFlex
                    , justifyContent spaceBetween
                    ]
                |> s_frame
                    (\lightmode visible ->
                        let
                            p =
                                if visible then
                                    0

                                else
                                    -120
                        in
                        [ backgroundColorWithLightmode lightmode colors.brandWhite
                        , Css.Transitions.transition
                            [ Css.Transitions.transform 200
                            ]
                        , displayFlex
                        , scaled 2 |> rem |> padding
                        , width <| calc (pct 100) minus (scaled 4 |> rem)
                        , translateY (pct p) |> transform
                        , shadowSm
                        , minHeight <| px 30

                        --, borderWidth (px 1)
                        --, borderStyle solid
                        --, colors.brandDark |> toCssColor |> borderColor
                        ]
                    )
                |> s_propertyBoxRule
                    [ borderWidth (px 0)
                    , scaled 1 |> rem |> margin
                    ]
                |> s_propertyBoxOutgoingTxs
                    (\lightmode ->
                        [ colorWithLightmode lightmode colors.brandRed
                        ]
                    )
                |> s_propertyBoxIncomingTxs
                    (\lightmode ->
                        [ colorWithLightmode lightmode colors.brandBase
                        ]
                    )
                |> s_propertyBoxTableLink
                    (\lightmode isActive ->
                        [ colorWithLightmode lightmode
                            (if isActive then
                                colors.brandBase

                             else
                                colors.brandLight
                            )
                        , hover
                            [ switchColor lightmode colors.brandBase |> toCssColor |> color
                            ]
                        , active
                            [ switchColor lightmode colors.brandBase |> toCssColor |> color
                            ]
                        ]
                    )
                |> s_copyLink
                    (\lightmode isActive ->
                        [ colorWithLightmode lightmode
                            (if isActive then
                                colors.brandBase

                             else
                                colors.brandLight
                            )
                        , hover
                            [ switchColor lightmode colors.brandBase |> toCssColor |> color
                            ]
                        , active
                            [ switchColor lightmode colors.brandBase |> toCssColor |> color
                            ]
                        ]
                    )
                |> s_longIdentifier [display block, fontFamily monospace]
                |> s_propertyBoxEntityId
                    (\lightmode ->
                        [ scaled 3 |> rem |> fontSize
                        , scaled 1 |> rem |> paddingLeft
                        , colors.greyDark
                            |> colorWithLightmode lightmode
                        ]
                    )
                |> s_loadingSpinner
                    [ scaled 3.5 |> rem |> height
                    , scaled 3.5 |> rem |> width
                    ]
                |> s_valueCell
                    [ scaled 1 |> rem |> paddingLeft
                    , scaled 1 |> rem |> paddingBottom
                    , textAlign right
                    , ex 30 |> width
                    ]
            )
        |> s_table
            (Table.default
                |> s_root
                    [ displayFlex
                    , flexDirection row
                    , overflowX auto
                    ]
                |> s_tableRoot
                    [ scaled 3 |> rem |> paddingX
                    , displayFlex
                    , flexDirection column

                    --, overflowX hidden
                    ]
                |> s_sidebar
                    (\lightmode ->
                        [ borderLeftWidth (px 1)
                        , borderStyle solid
                        , switchColor lightmode colors.greyLightest |> toCssColor |> borderColor
                        , scaled 2 |> rem |> paddingLeft
                        , scaled 1 |> rem |> paddingTop
                        ]
                    )
                |> s_sidebarIcon
                    (\lightmode active ->
                        [ cursor pointer
                        , scaled 2 |> rem |> paddingBottom
                        , (if active then
                            iconActive

                           else
                            iconInactive
                          )
                            |> switchColor lightmode
                            |> toCssColor
                            |> color
                        , hover
                            [ switchColor lightmode iconHovered |> toCssColor |> color
                            ]
                        ]
                    )
                |> s_filter
                    [ displayFlex
                    , justifyContent flexEnd
                    , padding (px 2)
                    ]
                |> s_filterInput
                    (\lightmode ->
                        [ inputStyle lightmode
                        , scaled 3 |> rem |> fontSize
                        , width <| ex 40
                        ]
                    )
                |> s_urlMaxLength 20
                |> s_table
                    [ scaled 1 |> rem |> padding
                    ]
                |> s_headCell
                    (\lightmode ->
                        [ tableCell
                        , rowHeight |> px |> height
                        , position sticky
                        , top <| px 0
                        , zIndex <| int 2
                        , backgroundColorWithLightmode lightmode colors.brandWhite
                        ]
                    )
                |> s_headRow
                    [ textAlign left
                    , fontWeight bold
                    ]
                |> s_headCellSortable
                    [ ( "cursor", "pointer" )
                    ]
                |> s_maxHeight 300
                |> s_rowHeight
                    rowHeight
                |> s_row
                    (\lightmode ->
                        [ nthChild "2n"
                            [ backgroundColorWithLightmode lightmode colors.brandLightest
                            ]
                        , rowHeight |> px |> height
                        ]
                    )
                |> s_cell
                    [ tableCell
                    ]
                |> s_numberCell
                    [ numberCell
                    ]
                |> s_valuesCell
                    (\lightmode isNegative ->
                        numberCell
                            :: (if isNegative then
                                    [ colorWithLightmode lightmode colors.brandRed ]

                                else
                                    []
                               )
                    )
                |> s_loadingSpinner
                    [ loadingSpinner
                    ]
                |> s_emptyHint
                    [ displayFlex
                    , flexGrow (int 1)
                    , alignItems center
                    , justifyContent center
                    ]
                |> s_tick
                    [ scaled 1 |> rem |> marginRight
                    ]
            )
        |> s_contextMenu
            (ContextMenu.default
                |> s_root
                    (\lightmode ->
                        [ backgroundColorWithLightmode lightmode colors.brandWhite
                        , shadowMd
                        ]
                    )
                |> s_option
                    (\lightmode ->
                        [ scaled 2 |> rem |> padding
                        , whiteSpace noWrap
                        , hover
                            [ switchColor lightmode backgroundHoverColor |> toCssColor |> backgroundColor
                            ]
                        ]
                    )
            )
        |> s_statusbar
            (Statusbar.default
                |> s_root
                    (\lightmode visible ->
                        [ switchColor lightmode colors.brandWhite |> toCssColor |> backgroundColor
                        , (if visible then
                            50

                           else
                            4
                          )
                            |> scaled
                            |> rem
                            |> minHeight
                        , scaled 50 |> rem |> maxHeight
                        , colors.greyDark |> colorWithLightmode lightmode
                        , scaled 3 |> rem |> fontSize
                        ]
                            ++ (if visible then
                                    [ scaled 2 |> rem |> padding
                                    , scaled 1 |> rem |> paddingLeft
                                    , Css.Transitions.transition
                                        [ Css.Transitions.minHeight 200
                                        ]
                                    , overflowY auto
                                    ]

                                else
                                    [ cursor pointer
                                    , displayFlex
                                    , justifyContent spaceBetween
                                    , alignItems center
                                    ]
                               )
                    )
                |> s_loadingSpinner
                    [ loadingSpinner
                    , padding (px 0)
                    , scaled 1 |> rem |> paddingRight
                    ]
                |> s_log
                    (\lightmode noerror ->
                        [ displayFlex
                        ]
                            ++ (if noerror then
                                    [ alignItems center
                                    ]

                                else
                                    [ colorWithLightmode lightmode colors.brandRed
                                    , fontWeight bold
                                    , scaled 4 |> rem |> fontSize
                                    ]
                               )
                    )
                |> s_logIcon
                    (\_ _ -> [ scaled 1 |> rem |> paddingRight ])
                |> s_close
                    (\lightmode ->
                        [ colors.brandText |> colorWithLightmode lightmode
                        , backgroundColor transparent
                        , border (px 0)
                        , position absolute
                        , scaled 2 |> rem |> top
                        , scaled 4 |> rem |> right
                        , cursor pointer
                        ]
                    )
            )
        |> s_custom
            -- need to put these special references in separate string expressions to make the vite resolution work
            ("[VITE_PLUGIN_ELM_ASSET:/themes/Iknaio/fonts/Octarine-Light/fonts.css]"
                ++ " ::placeholder { color: inherit; opacity: 0.5 }"
            )


scaled : Float -> Float
scaled =
    (*) 0.22


fontHairline : Style
fontHairline =
    fontWeight (int 100)


fontNormal : Style
fontNormal =
    fontWeight (int 300)


fontBold : Style
fontBold =
    fontWeight (int 500)


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



--marginX : Length compatibleB unitsB -> Style


marginX x =
    batch
        [ marginLeft x
        , marginRight x
        ]


borderRadiusSmValue : Float
borderRadiusSmValue =
    0.5


borderRadiusSmRaw : ( String, String )
borderRadiusSmRaw =
    ( "border-radius"
    , String.fromFloat (scaled borderRadiusSmValue) ++ "rem"
    )


borderRadiusSm : Style
borderRadiusSm =
    property
        (Tuple.first borderRadiusSmRaw)
        (Tuple.second borderRadiusSmRaw)


inputStyleRaw : Bool -> Maybe Float -> List ( String, String )
inputStyleRaw lm len =
    [ ( "transition"
      , "color "
            ++ String.fromInt Util.Theme.duration
            ++ "ms,"
            ++ "background-color "
            ++ String.fromInt Util.Theme.duration
            ++ "ms"
      )
    , ( "background-color", Color.toCssString <| switchColor lm colors.greyLighter )
    , ( "color", Color.toCssString <| Util.Theme.switchColor lm colors.black )
    , borderRadiusSmRaw
    , ( "border", "0" )
    , ( "padding", (scaled 1 |> String.fromFloat) ++ "rem" )
    , ( "margin-bottom", (scaled 1 |> String.fromFloat) ++ "rem" )
    , ( "display", "block" )
    ]
        ++ (Maybe.map (\l -> [ ( "width", String.fromFloat l ++ "ex" ) ]) len
                |> Maybe.withDefault []
           )


inputStyle : Bool -> Style
inputStyle lightmode =
    inputStyleRaw lightmode Nothing
        |> List.map (\( k, v ) -> property k v)
        |> batch


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


shadowSm : Style
shadowSm =
    property "box-shadow" "1px 1px 1px 0 rgba(0, 0, 0, .1)"


shadowMd : Style
shadowMd =
    property "box-shadow" "0 4px 8px 0 rgba(0, 0, 0, .12), 0 2px 4px 0 rgba(0, 0, 0, .08)"


tableCell : Style
tableCell =
    [ scaled 1 |> rem |> padding
    , whiteSpace noWrap
    , verticalAlign middle
    ]
        |> batch


numberCell : Style
numberCell =
    [ tableCell
    , textAlign right
    ]
        |> batch


loadingSpinner : Style
loadingSpinner =
    [ scaled spinnerHeight |> rem |> height
    , scaled spinnerHeight |> rem |> width
    , scaled spinnerPadding |> rem |> padding
    ]
        |> batch


fillBlack : Bool -> List Style
fillBlack lightmode =
    [ Util.Theme.switchColor lightmode colors.black
        |> Color.toCssString
        |> property "fill"
    ]


iconDisabled : Theme.SwitchableColor
iconDisabled =
    colors.greyLighter


iconInactive : Theme.SwitchableColor
iconInactive =
    colors.brandLight


iconActive : Theme.SwitchableColor
iconActive =
    colors.brandBase


iconHovered : Theme.SwitchableColor
iconHovered =
    colors.brandBase


backgroundHoverColor : Theme.SwitchableColor
backgroundHoverColor =
    colors.brandLighter


rowHeight : Float
rowHeight =
    scaled 90
