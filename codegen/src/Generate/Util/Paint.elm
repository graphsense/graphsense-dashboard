module Generate.Util.Paint exposing (getBasePaint, getOpacity, toColor, toRGBA, toStylesString)

import Api.Raw exposing (BasePaint, Paint(..), RGBA)
import Elm
import Gen.Color as Color
import Generate.Util.RGBA as RGBA
import Types exposing (ColorMap)


toStyles : ColorMap -> List Paint -> Maybe Elm.Expression
toStyles colorMap =
    toRGBA
        >> Maybe.map (RGBA.toStyles colorMap)


toStylesString : ColorMap -> List Paint -> Maybe String
toStylesString colorMap =
    toRGBA
        >> Maybe.map (RGBA.toStylesString colorMap)



-->> Maybe.map Elm.string


toRGBA : List Paint -> Maybe RGBA
toRGBA =
    List.head
        >> Maybe.andThen
            (\p ->
                case p of
                    PaintSolidPaint { color, basePaint } ->
                        { color
                            | a = color.a * Maybe.withDefault 1 basePaint.opacity
                        }
                            |> Just

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


toColor : List Paint -> Maybe Elm.Expression
toColor =
    toRGBA
        >> Maybe.map (\{ r, g, b, a } -> Color.rgba r g b a)


getOpacity : Maybe (List Paint) -> Float
getOpacity =
    Maybe.andThen getBasePaint
        >> Maybe.andThen .opacity
        >> Maybe.withDefault 1
