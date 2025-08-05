module Util.Css exposing (alignItemsStretch, overrideBlack, overwriteGreen300, overwriteIconWithDisabledColor, overwritePrimary, spread, zIndexMain, zIndexMainValue)

import Css exposing (Style, int, zIndex)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Theme.Colors


zIndexMain : Style
zIndexMain =
    zIndex <| int zIndexMainValue


zIndexMainValue : Int
zIndexMainValue =
    50


overrideBlack : String -> Style
overrideBlack =
    -- that's a hacky workaround to apply color overrides
    Css.property Theme.Colors.sidebarNeutral_name


overwritePrimary : String -> Style
overwritePrimary =
    -- that's a hacky workaround to apply color overrides
    Css.property Theme.Colors.brandPrimary_name


overwriteGreen300 : String -> Style
overwriteGreen300 =
    -- that's a hacky workaround to apply color overrides
    Css.property Theme.Colors.green300_name


overwriteIconWithDisabledColor : List Style
overwriteIconWithDisabledColor =
    [ overwritePrimary Theme.Colors.greyBlue100 |> Css.important
    , overwriteGreen300 Theme.Colors.greyBlue100 |> Css.important
    ]


alignItemsStretch : Attribute msg
alignItemsStretch =
    css [ Css.alignItems Css.stretch |> Css.important ]


spread : Attribute msg
spread =
    css
        [ Css.flexGrow <| Css.num 1
        , Css.justifyContent Css.spaceBetween
        , Css.width <| Css.pct 100
        ]
