module Generate.Svg.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Svg.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> HasGeometryTrait -> List Elm.Expression
toStyles colorMap node =
    MinimalFillsTrait.toStyles colorMap node.minimalFillsTrait
        |> (++) (toStroke colorMap node.strokes)
        |> m (String.fromFloat >> Css.property "stroke-width") node.strokeWeight


toStroke : ColorMap -> Maybe (List Paint) -> List Elm.Expression
toStroke colorMap paints =
    []
        |> a
            (Paint.toStylesString colorMap
                >> Maybe.map (Css.property "stroke")
            )
            {- already covered by color alpha
               paints
                  |> a
                      (Paint.getBasePaint
                          >> Maybe.andThen .opacity
                          >> Maybe.map String.fromFloat
                          >> Maybe.map (Css.property "stroke-opacity")
                      )
            -}
            paints
