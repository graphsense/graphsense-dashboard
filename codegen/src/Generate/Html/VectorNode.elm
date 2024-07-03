module Generate.Html.VectorNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Svg.VectorNode
import Types exposing (Config)


toExpressions : Config -> VectorNode -> List Elm.Expression
toExpressions config node =
    Generate.Svg.VectorNode.toExpressions config node
        |> Gen.Svg.Styled.svg
            [ node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.width
                |> String.fromFloat
                |> Attributes.width
            , node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox.height
                |> String.fromFloat
                |> Attributes.height
            ]
        |> List.singleton
