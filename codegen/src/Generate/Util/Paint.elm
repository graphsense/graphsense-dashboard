module Generate.Util.Paint exposing (getBasePaint, toCss, toCssString, toRGBA)

import Api.Raw exposing (BasePaint, Paint(..), RGBA)
import Elm
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
