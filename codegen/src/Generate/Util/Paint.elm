module Generate.Util.Paint exposing (getBasePaint, toColor, toCss, toCssString, getOpacity)

import Api.Raw exposing (BasePaint, Paint(..), RGBA)
import Elm
import Gen.Color as Color
import Generate.Util.RGBA as RGBA


toCss : List Paint -> Maybe Elm.Expression
toCss =
    toRGBA
        >> Maybe.map RGBA.toCss


toCssString : List Paint -> Maybe Elm.Expression
toCssString =
    toRGBA
        >> Maybe.map RGBA.toCssString


toRGBA : List Paint -> Maybe RGBA
toRGBA =
    List.head
        >> Maybe.andThen
            (\p ->
                case p of
                    PaintSolidPaint { color } ->
                        Just color

                    _ ->
                        Nothing
            )


getBasePaint : List Paint -> Maybe BasePaint
getBasePaint =
    List.head
        >> Maybe.map
            (\p ->
                case p of
                    PaintSolidPaint { basePaint } ->
                        basePaint

                    PaintImagePaint { basePaint } ->
                        basePaint

                    PaintGradientPaint { basePaint } ->
                        basePaint
            )


toColor : Maybe (List Paint) -> Maybe Elm.Expression
toColor =
    Maybe.andThen toRGBA
        >> Maybe.map (\{ r, g, b, a } -> Color.rgba r g b a)


getOpacity : Maybe (List Paint) -> Float
getOpacity =
    Maybe.andThen getBasePaint
        >> Maybe.andThen .opacity
        >> Maybe.withDefault 1
