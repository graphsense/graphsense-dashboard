module Generate.Html.VectorNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.DefaultShapeTraits
import RecordSetter exposing (s_styles)
import Types exposing (ColorMap, Config, Details)


toExpressions : Config -> String -> VectorNode -> List Elm.Expression
toExpressions config componentName =
    .cornerRadiusShapeTraits
        >> DefaultShapeTraits.toExpressions config componentName


toStyles : VectorNode -> List Elm.Expression
toStyles node =
    []



{-
   [ Css.position Css.absolute
   , node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.y
       |> Css.px
       |> Css.top
   , node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.x
       |> Css.px
       |> Css.left
   ]
-}


toDetails : ColorMap -> VectorNode -> Details
toDetails colorMap node =
    Generate.Svg.DefaultShapeTraits.toDetails colorMap node.cornerRadiusShapeTraits
        |> s_styles (toStyles node)
