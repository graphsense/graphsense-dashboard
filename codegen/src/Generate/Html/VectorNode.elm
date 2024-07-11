module Generate.Html.VectorNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Svg.VectorNode
import RecordSetter exposing (s_styles)
import Types exposing (Config, Details)
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits


toExpressions : Config -> ( String, String ) -> VectorNode -> List Elm.Expression
toExpressions config componentNameId =
    .cornerRadiusShapeTraits
    >> DefaultShapeTraits.toExpressions config componentNameId

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
    Generate.Svg.VectorNode.toDetails node
        |> s_styles (toStyles node)
