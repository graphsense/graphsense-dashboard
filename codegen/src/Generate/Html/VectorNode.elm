module Generate.Html.VectorNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Svg.VectorNode
import RecordSetter exposing (s_styles)
import Types exposing (Config, Details)


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


toExpressions : Config -> ( String, String ) -> VectorNode -> List Elm.Expression
toExpressions config componentNameId node =
    Generate.Svg.VectorNode.toExpressions config componentNameId node
        |> Gen.Svg.Styled.svg
            [ node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.width
                |> String.fromFloat
                |> Attributes.width
            , node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.height
                |> String.fromFloat
                |> Attributes.height
            ]
        |> List.singleton


toDetails : VectorNode -> Details
toDetails node =
    Generate.Svg.VectorNode.toDetails node
        |> s_styles (toStyles node)
