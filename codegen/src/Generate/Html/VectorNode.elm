module Generate.Html.VectorNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.DefaultShapeTraits
import RecordSetter exposing (s_styles)
import Types exposing (Config, Details)


toExpressions : Config -> String -> VectorNode -> List Elm.Expression
toExpressions config componentName =
    .cornerRadiusShapeTraits
        >> DefaultShapeTraits.toExpressions config componentName


toStyles : VectorNode -> List Elm.Expression
toStyles node =
    [ Css.position Css.absolute
    , node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.y
        |> Css.px
        |> Css.top
    , node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.x
        |> Css.px
        |> Css.left
    ]


toDetails : VectorNode -> Details
toDetails node =
    Generate.Svg.DefaultShapeTraits.toDetails node.cornerRadiusShapeTraits
        |> s_styles (toStyles node)
