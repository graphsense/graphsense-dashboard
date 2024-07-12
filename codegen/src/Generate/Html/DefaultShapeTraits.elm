module Generate.Html.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.HasGeometryTrait as HasGeometryTrait
import Generate.Svg.DefaultShapeTraits
import Generate.Util exposing (..)
import Types exposing (Config, Details)
import Generate.Html.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Html.HasLayoutTrait as HasLayoutTrait


toStyles : DefaultShapeTraits -> List Elm.Expression
toStyles node =
        HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait
        ++ HasGeometryTrait.toStyles node.hasGeometryTrait


toDetails : { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails node =
    Common.toDetails (toStyles node.defaultShapeTraits) node


toExpressions : Config -> ( String, String ) -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toExpressions config componentNameId node =
    let
        bbox =
            node.defaultShapeTraits.absoluteBoundingBox
    in
    Generate.Svg.DefaultShapeTraits.toExpressions config componentNameId node
        |> Gen.Svg.Styled.svg
            [ max 3 bbox.width
                |> String.fromFloat
                |> Attributes.width
            , max 3 bbox.height
                |> String.fromFloat
                |> Attributes.height
            , [ bbox.x
              , bbox.y
              , max 1 bbox.width
              , max 1 bbox.height
              ]
                |> List.map String.fromFloat
                |> String.join " "
                |> Attributes.viewBox
            ]
        |> List.singleton
