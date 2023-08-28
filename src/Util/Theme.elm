module Util.Theme exposing (..)

import Color
import Css
import Css.Transitions
import Theme.Theme as Theme
import Tuple exposing (..)
import Util.View as Util


duration =
    500


color : ( Float -> Css.Transitions.Transition, Css.Color -> Css.Style )
color =
    ( Css.Transitions.color, Css.color )


backgroundColor : ( Float -> Css.Transitions.Transition, Css.Color -> Css.Style )
backgroundColor =
    ( Css.Transitions.backgroundColor, Css.backgroundColor )


borderColor : ( Float -> Css.Transitions.Transition, Css.Color -> Css.Style )
borderColor =
    ( Css.Transitions.borderColor, Css.borderColor )


colorWithLightmode : Bool -> Theme.SwitchableColor -> Css.Style
colorWithLightmode lm c =
    withLightmode [ ( color, c ) ] lm


backgroundColorWithLightmode : Bool -> Theme.SwitchableColor -> Css.Style
backgroundColorWithLightmode lm c =
    withLightmode [ ( backgroundColor, c ) ] lm


borderColorWithLightmode : Bool -> Theme.SwitchableColor -> Css.Style
borderColorWithLightmode lm c =
    withLightmode [ ( borderColor, c ) ] lm


color_backgroundColorWithLightmode : Bool -> Theme.SwitchableColor -> Theme.SwitchableColor -> Css.Style
color_backgroundColorWithLightmode lm c bg =
    withLightmode [ ( color, c ), ( backgroundColor, bg ) ] lm


borderColor_backgroundColorWithLightmode : Bool -> Theme.SwitchableColor -> Theme.SwitchableColor -> Css.Style
borderColor_backgroundColorWithLightmode lm c bg =
    withLightmode [ ( borderColor, c ), ( backgroundColor, bg ) ] lm


withLightmode : List ( ( Float -> Css.Transitions.Transition, Css.Color -> Css.Style ), Theme.SwitchableColor ) -> Bool -> Css.Style
withLightmode attributes lm =
    (attributes
        |> List.map (\( ( attr, _ ), _ ) -> attr duration)
        |> Css.Transitions.transition
    )
        :: (attributes
                |> List.map
                    (\( ( _, attr ), c ) -> switchColor lm c |> Util.toCssColor |> attr)
           )
        |> Css.batch


switchColor : Bool -> Theme.SwitchableColor -> Color.Color
switchColor lm c =
    if lm then
        c.light

    else
        c.dark


setAlpha : Float -> Theme.SwitchableColor -> Theme.SwitchableColor
setAlpha pct col =
    let
        updAlpha rgba =
            { rgba | alpha = pct }
    in
    { dark = Color.toRgba col.dark |> updAlpha |> Color.fromRgba
    , light = Color.toRgba col.light |> updAlpha |> Color.fromRgba
    }
