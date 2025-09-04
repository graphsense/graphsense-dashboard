module Generate.Svg.MinimalStrokesTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Svg.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> MinimalStrokesTrait -> List Elm.Expression
toStyles colorMap node =
    []
        |> m (Paint.toStylesString colorMap >> Maybe.withDefault "transparent" >> Css.property "stroke") node.strokes
        |> m (String.fromFloat >> Css.property "stroke-width") node.strokeWeight
        --|> a MinimalFillsTrait.opacity node.strokes
