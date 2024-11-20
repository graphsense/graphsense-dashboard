module Generate.Html.HasEffectsTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Util.RGBA as RGBA
import String.Format as Format
import Types exposing (ColorMap)


toStyles : ColorMap -> HasEffectsTrait -> List Elm.Expression
toStyles colorMap node =
    node.effects
        |> List.filterMap (effectToStyle colorMap)


effectToStyle : ColorMap -> Effect -> Maybe Elm.Expression
effectToStyle colorMap effect =
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
            baseShadowToStyle colorMap False baseShadowEffect

        EffectInnerShadowEffect { baseShadowEffect } ->
            baseShadowToStyle colorMap True baseShadowEffect


baseShadowToStyle : ColorMap -> Bool -> BaseShadowEffect -> Maybe Elm.Expression
baseShadowToStyle colorMap inset { color, offset, radius, spread, visible } =
    if not visible then
        Nothing

    else
        let
            boxShadowInset =
                if inset then
                    "inset"

                else
                    ""

            px n =
                String.fromFloat n ++ "px"
        in
        String.join " "
            [ boxShadowInset
            , px offset.x
            , px offset.y
            , px radius
            , spread |> Maybe.withDefault 0 |> px
            , RGBA.toStylesString colorMap color
            ]
            |> Css.property "box-shadow"
            |> Just
