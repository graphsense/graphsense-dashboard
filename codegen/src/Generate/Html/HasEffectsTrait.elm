module Generate.Html.HasEffectsTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Util.RGBA as RGBA
import String.Format as Format


toStyles : HasEffectsTrait -> List Elm.Expression
toStyles node =
    node.effects
        |> List.filterMap effectToStyle


effectToStyle : Effect -> Maybe Elm.Expression
effectToStyle effect =
    case effect of
        EffectBlurEffect blurEffect ->
            if blurEffect.visible then
                "blur({{ }}px)"
                    |> Format.value (String.fromFloat blurEffect.radius)
                    |> Css.property "filter"
                    |> Just

            else
                Nothing

        EffectDropShadowEffect { baseShadowEffect } ->
            baseShadowToStyle False baseShadowEffect

        EffectInnerShadowEffect { baseShadowEffect } ->
            baseShadowToStyle True baseShadowEffect


baseShadowToStyle : Bool -> BaseShadowEffect -> Maybe Elm.Expression
baseShadowToStyle inset { color, offset, radius, spread, visible } =
    if not visible then
        Nothing

    else
        let
            boxShadow =
                if inset then
                    Css.boxShadow6 Css.inset

                else
                    Css.boxShadow5
        in
        RGBA.toStyles color
            |> boxShadow
                (Css.px offset.x)
                (Css.px offset.y)
                (Css.px radius)
                (spread |> Maybe.withDefault 0 |> Css.px)
            |> Just
