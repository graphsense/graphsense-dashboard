module Iknaio exposing (theme)

import Color exposing (rgb255)
import Css exposing (..)
import Css.Transitions
import Iknaio.ColorScheme exposing (..)
import Iknaio.DesignToken as DesignToken
import Iknaio.DesignTokens exposing (..)
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
import Theme.Landingpage as Landingpage
import Theme.Pathfinder as Pathfinder
import Theme.Search as Search
import Theme.Stats as Stats
import Theme.Statusbar as Statusbar
import Theme.SwitchableColor as Theme
import Theme.Table as Table
import Theme.Theme as Theme exposing (Theme)
import Theme.User as User
import Tuple
import Util.Theme
    exposing
        ( backgroundColorWithLightmode
        , borderColorWithLightmode
        , borderColor_backgroundColorWithLightmode
        , colorWithLightmode
        , color_backgroundColorWithLightmode
        , setAlpha
        , switchColor
        )
import Util.View exposing (toCssColor)


duration : Int
duration =
    500


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
    , brandBase = { dark = rgb255 132 165 194, light = rgb255 107 203 186 }
    , brandLight = { dark = rgb255 70 109 145, light = rgb255 151 219 207 }
    , brandLighter = { dark = rgb255 7 69 116, light = rgb255 210 236 237 }
    , brandLightest = { dark = rgb255 5 50 84, light = rgb255 248 250 252 }
    , brandRed = { dark = rgb255 185 86 86, light = rgb255 204 106 66 }
    , brandRedLight = { dark = rgb255 241 182 182, light = rgb255 238 204 190 }
    , brandWhite = { dark = rgb255 3 31 53, light = rgb255 255 255 255 }
    }


fontFam : List String
fontFam =
    ["Roboto", "system-ui", " BlinkMacSystemFont", " -apple-system", " Segoe UI", " Roboto", " Oxygen", " Ubuntu", " Cantarell", " Fira Sans", " Droid Sans", " Helvetica Neue", " sans-serif" ]


headingFontFamilies : List String
headingFontFamilies =
    [ "Roboto", "Conv_Octarine-Light" ]

monospacedFontFamilies : List String
monospacedFontFamilies = ["RobotoMono", "monospace"]

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
                , DesignToken.variables lightmode designTokens
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
            [ fontFamilies headingFontFamilies
            , scaled 5 |> rem |> fontSize
            , fontWeight bold
            , letterSpacingWide
            , display inline
            , scaled 2 |> rem |> marginLeft
            ]
        |> s_heading2
            [ fontFamilies headingFontFamilies
            , letterSpacingWide
            , scaled 6 |> rem |> fontSize
            , fontWeight bold
            , scaled 1 |> rem |> paddingTop
            , marginBottom <| rem <| 1
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
            [ width <| px 190
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
        |> s_sidebarIconBottom
            (\lightmode active ->
                [ colorWithLightmode lightmode iconInactive
                , scaled 5 |> rem |> fontSize
                , scaled 4 |> rem |> padding
                , position absolute
                , bottom (px 15)
                , left (px 0)
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
        |> s_sidebarRule
            (\lightmode ->
                [ Css.width (pct 50)
                , borderWidth (px 0.5)

                --, scaled 1 |> rem |> margin
                , colors.greyLight |> colorWithLightmode lightmode
                , opacity <| num 0.5
                ]
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
                , textDecoration underline
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
        |> s_copyIcon
            (\lightmode ->
                [ colorWithLightmode lightmode colors.brandBase
                , hover
                    [ switchColor lightmode colors.brandBase |> toCssColor |> color
                    ]
                , active
                    [ switchColor lightmode colors.brandDark |> toCssColor |> color
                    ]
                ]
            )
        |> s_longIdentifier [ fontFamilies monospacedFontFamilies ]
        |> s_hint
            (\lightmode ->
                [ colorWithLightmode lightmode colors.greyDark
                , scaled 2.7 |> rem |> fontSize
                ]
            )
        |> s_frame
            (\_ ->
                let
                    p =
                        scaled 5
                in
                [ padding (rem p)
                , calc (pct 100) minus (rem p)
                    |> width
                ]
            )
        |> s_box
            (\lightmode ->
                [ scaled 7 |> rem |> padding
                , backgroundColorWithLightmode lightmode colors.white
                ]
            )
        |> s_stats
            (Stats.default
                |> s_stats
                    [ scaled -statsMargin |> rem |> marginLeft ]
                |> s_currency
                    (\lightmode ->
                        [ backgroundColorWithLightmode lightmode colors.greyLight
                        , scaled statsMargin |> rem |> margin
                        , borderRadiusSm
                        ]
                    )
                |> s_tokenBadgeStyle
                    (\lightmode ->
                        [ --backgroundColorWithLightmode lightmode colors.brandLightest
                          borderColorWithLightmode lightmode colors.brandDark
                        , borderRadius <| px 5
                        , borderStyle solid
                        , borderWidth (px 1)
                        , px 5 |> marginRight
                        , px 2 |> paddingLeft
                        , px 2 |> paddingRight
                        , px 1 |> paddingTop
                        , px 1 |> paddingBottom
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
                        , fontFamilies headingFontFamilies
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
        |> s_landingpage
            (Landingpage.default
                |> s_root
                    [ displayFlex
                    , width <| pct 100
                    , height <| pct 100
                    , flexDirection column
                    , alignItems center
                    , scaled 20 |> rem |> marginTop
                    ]
                |> s_searchRoot
                    [ paddingTop <| rem <| scaled 5
                    ]
                |> s_frame
                    (\lightmode ->
                        [ backgroundColorWithLightmode lightmode colors.white
                        , displayFlex
                        , alignItems center
                        , flexDirection column
                        , scaled 7 |> rem |> padding
                        , borderRadiusSm
                        , shadowSm
                        ]
                    )
                |> s_rule
                    (\lightmode ->
                        [ width <| pct 50
                        , colorWithLightmode lightmode colors.grey
                        , paddingTop <| rem <| scaled 8
                        , paddingBottom <| rem <| scaled 5
                        ]
                    )
                |> s_ruleColor colors.grey
                |> s_loadBox
                    (\lightmode ->
                        [ scaled 4 |> rem |> padding
                        , borderRadiusSm
                        , border zero
                        , backgroundColorWithLightmode lightmode colors.greyLighter
                        , hover
                            [ backgroundColorWithLightmode lightmode colors.brandLighter
                            ]
                        , disabled
                            [ colorWithLightmode lightmode colors.brandLight
                            ]
                        , displayFlex
                        , flexDirection column
                        , alignItems center
                        , cursor pointer
                        ]
                    )
                |> s_loadBoxIcon
                    (\lightmode ->
                        [ colorWithLightmode lightmode colors.brandDark
                        , paddingBottom <| rem <| scaled 4
                        , fontSize <| rem <| scaled 7
                        ]
                    )
                |> s_loadBoxText
                    (\_ ->
                        [ textDecoration none
                        ]
                    )
                |> s_exampleLinkBox
                    (\_ ->
                        [ paddingTop <| rem <| scaled 4 ]
                    )
            )
        |> s_search
            (Search.default
                |> s_frame
                    [ scaled 1 |> rem |> marginRight
                    , fontFamilies monospacedFontFamilies
                    ]
                |> s_form
                    [ scaled 3 |> rem |> fontSize
                    , scaled 8 |> rem |> height
                    ]
                |> s_textarea
                    (\lightmode input ->
                        [ scaled 5 |> rem |> height
                        , inputStyle lightmode
                        , marginBottom zero
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
                        , hover (resultLineHighlighted lightmode)
                        , overflow hidden
                        , textOverflow ellipsis
                        , whiteSpace noWrap
                        ]
                    )
                |> s_resultLineHighlighted resultLineHighlighted
                |> s_resultLineIcon
                    [ opacity <| num 0.5
                    , scaled 1 |> rem |> paddingRight
                    ]
                |> s_button
                    (\lightmode ->
                        [ color_backgroundColorWithLightmode lightmode colors.brandDark colors.greyLighter
                        , property "box-shadow" "none"
                        , scaled 1 |> rem |> paddingY
                        , scaled 2 |> rem |> paddingX
                        ]
                    )
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
                        [ fontWeight normal
                        , fontSize <| rem <| scaled 4
                        , textAlign center
                        , textDecoration none
                        , scaled 2 |> rem |> paddingY
                        , scaled 5 |> rem |> paddingX
                        , scaled 1 |> rem |> marginX
                        , shadowSm
                        , borderRadiusSm
                        , border zero
                        , hover
                            [ backgroundColorWithLightmode lightmode colors.brandLighter
                            ]
                        ]
                    )
                |> s_neutral
                    (\lightmode ->
                        [ color_backgroundColorWithLightmode lightmode colors.brandText colors.greyLighter
                        , disabled
                            [ colorWithLightmode lightmode colors.brandLight
                            ]
                        ]
                    )
                |> s_primary
                    (\lightmode ->
                        [ color_backgroundColorWithLightmode lightmode colors.brandText colors.brandLight
                        , hover
                            [ backgroundColorWithLightmode lightmode colors.brandBase
                            ]
                        ]
                    )
                |> s_danger
                    (\lightmode ->
                        [ color_backgroundColorWithLightmode lightmode colors.brandRed colors.brandWhite
                        ]
                    )
                |> s_disabled
                    (\lightmode ->
                        [ color_backgroundColorWithLightmode lightmode colors.grey colors.greyLight
                        , hover
                            [ color_backgroundColorWithLightmode lightmode colors.grey colors.greyLight
                            ]
                        ]
                    )
                |> s_iconButton
                    (\_ ->
                        [ width <| px 16
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
                    ]
                |> s_singleButton
                    [ displayFlex
                    , justifyContent center
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
        |> s_pathfinder
            (Pathfinder.default
                |> s_addressRadius 30
                |> s_txRadius 5
                |> s_address
                    [ DesignToken.init
                        |> DesignToken.token "stroke" addressStrokeColor
                        |> DesignToken.token "fill" addressFillColor
                        |> DesignToken.withDuration duration
                        |> DesignToken.css
                    ]
                |> s_addressLabel
                    [ DesignToken.init
                        |> DesignToken.token "fill" addressFontColor
                        |> DesignToken.token "font-weight" addressFontWeight
                        |> DesignToken.css
                    ]
                |> s_tx
                    [ DesignToken.init
                        |> DesignToken.token "stroke" txStrokeColor
                        |> DesignToken.token "fill" txFillColor
                        |> DesignToken.withDuration duration
                        |> DesignToken.css
                    ]
                |> s_edgeColor (DesignToken.toVariable edgeUtxoStrokeColor)
                |> s_outEdgeColor (DesignToken.toVariable edgeUtxoOutStrokeColor)
                |> s_inEdgeColor (DesignToken.toVariable edgeUtxoInStrokeColor)
                |> s_edge
                    [ DesignToken.init
                        |> DesignToken.token "stroke" edgeUtxoStrokeColor
                        |> DesignToken.token "stroke-width" edgeUtxoStrokeWidth
                        |> DesignToken.withDuration duration
                        |> DesignToken.css
                    ]
                |> s_edgeLabel
                    [ DesignToken.init
                        |> DesignToken.token "fill" edgeLabelFontColor
                        |> DesignToken.token "font-weight" edgeLabelFontWeight
                        |> DesignToken.css
                    ]
            )
        |> s_graph
            (Graph.default
                |> s_contextMenuRule
                    (\lightmode ->
                        [ borderWidth (px 0.5)
                        , scaled 1 |> rem |> margin
                        , colors.greyLight |> colorWithLightmode lightmode
                        , opacity <| num 0.5
                        ]
                    )
                |> s_categoryToColor
                    (\category ->
                        case category of
                            "exchange" ->
                                color0

                            "coinjoin" ->
                                color3

                            "perpetrator" ->
                                color1

                            "defi" ->
                                color2

                            "miner" ->
                                color4

                            "payment_processor" ->
                                color0

                            "user" ->
                                color5

                            "gambling" ->
                                color6

                            "defi_lending" ->
                                color7

                            "market" ->
                                color8

                            "mixing_service" ->
                                color9

                            "defi_dex" ->
                                color9

                            "donation" ->
                                color9

                            "service" ->
                                color9

                            "wallet_service" ->
                                color9

                            "hosting" ->
                                color9

                            "shop" ->
                                color9

                            "entity" ->
                                color9

                            "organization" ->
                                color9

                            "vpn" ->
                                color9

                            "faucet" ->
                                color9

                            "defi_bridge" ->
                                color9

                            "ico_wallet" ->
                                color9

                            "atm" ->
                                color9

                            "mining_service" ->
                                color9

                            _ ->
                                defaultColor
                    )
                |> (\graph ->
                        s_highlightsColorScheme
                            (colorScheme
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
                    defaultColor
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
                        [ fontFamilies monospacedFontFamilies
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
                    (\_ ->
                        [ scaled 1 |> rem |> paddingLeft
                        ]
                    )
                |> s_propertyBoxRow
                    (\lightmode active ->
                        hover
                            [ backgroundColorWithLightmode lightmode colors.brandLightest
                            ]
                            :: (if active then
                                    [ backgroundColorWithLightmode lightmode colors.brandLightest
                                    ]

                                else
                                    []
                               )
                    )
                |> s_propertyBoxKey
                    [ fontBold
                    , scaled 2 |> rem |> paddingRight
                    , scaled 0.5 |> rem |> paddingBottom
                    , scaled 0.5 |> rem |> paddingTop
                    , whiteSpace noWrap
                    ]
                |> s_propertyBoxValue
                    [ fontNormal
                    ]
                |> s_propertyBoxValueInner
                    [ displayFlex
                    , justifyContent spaceBetween
                    , whiteSpace noWrap
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
                        , width <| calc (pct 100) minus (scaled 3 |> rem)
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
                |> s_tableSeparator
                    (\lightmode ->
                        [ borderLeftWidth (px 1)
                        , borderStyle solid
                        , switchColor lightmode colors.greyLightest |> toCssColor |> borderColor
                        , scaled 1 |> rem |> paddingLeft
                        , scaled 1 |> rem |> marginLeft
                        ]
                    )
            )
        |> s_table
            (Table.default
                |> s_sidebar
                    (\lightmode ->
                        [ borderLeftWidth (px 1)
                        , borderStyle solid
                        , switchColor lightmode colors.greyLightest |> toCssColor |> borderColor
                        , scaled 2 |> rem |> marginLeft
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
                        , paddingTop zero
                        , position sticky
                        , top <| px 0
                        , zIndex <| int 2
                        , backgroundColorWithLightmode lightmode colors.brandWhite
                        ]
                    )
                |> s_headRow
                    [ textAlign left
                    , fontBold
                    ]
                |> s_headCellSortable
                    [ cursor pointer
                    ]
                |> s_maxHeight 300
                |> s_rowHeight
                    rowHeight
                |> s_row
                    (\lightmode ->
                        [ nthChild "2n"
                            [ backgroundColorWithLightmode lightmode colors.greyLighter
                            ]
                        , nthChild "2n+1"
                            [ colors.greyLighter
                                |> setAlpha 0.3
                                |> backgroundColorWithLightmode lightmode
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
                |> s_info
                    (\lightmode ->
                        [ scaled 1 |> rem |> padding
                        , color_backgroundColorWithLightmode lightmode colors.brandText colors.brandWhite
                        ]
                    )
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
                        displayFlex
                            :: (if noerror then
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


marginX : { compatible | value : String, lengthOrAuto : Compatible } -> Style
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


resultLineHighlighted : Bool -> List Style
resultLineHighlighted lightmode =
    [ backgroundColorWithLightmode lightmode colors.brandLighter
    ]
