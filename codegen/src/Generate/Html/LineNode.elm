module Generate.Html.LineNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Svg.DefaultShapeTraits
import RecordSetter exposing (s_styles)
import Types exposing (ColorMap, Details)


toStyles : LineNode -> List Elm.Expression
toStyles node =
    []



{-
   [ Css.position Css.absolute
   , node.defaultShapeTraits.absoluteBoundingBox.y
       |> Css.px
       |> Css.top
   , node.defaultShapeTraits.absoluteBoundingBox.x
       |> Css.px
       |> Css.left
   ]
-}


toDetails : ColorMap -> LineNode -> Details
toDetails colorMap node =
    Generate.Svg.DefaultShapeTraits.toDetails colorMap node
        |> s_styles (toStyles node)
