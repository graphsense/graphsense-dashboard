module Generate.Svg.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Svg.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util.Paint as Paint
import Generate.Util exposing (..)


toStyles : HasGeometryTrait -> List Elm.Expression
toStyles node =
    MinimalFillsTrait.toStyles node.minimalFillsTrait
        |> (++) (toStroke node.strokes)
        |> m (String.fromFloat >> Css.property "stroke-width") node.strokeWeight


toStroke : Maybe (List Paint) -> List Elm.Expression
toStroke paints =
    []
        |> a
            (Paint.toStylesString
                >> Maybe.map (Css.call_.property (Elm.string "stroke"))
            )
            paints
        |> a
            (Paint.getBasePaint
                >> Maybe.andThen .opacity
                >> Maybe.map String.fromFloat
                >> Maybe.map (Css.property "stroke-opacity")
            )
            paints
