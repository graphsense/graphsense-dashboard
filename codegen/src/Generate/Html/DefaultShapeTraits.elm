module Generate.Html.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Html.HasGeometryTrait as HasGeometryTrait
import Generate.Svg.DefaultShapeTraits
import Generate.Util exposing (..)
import String.Format as Format
import Types exposing (Config, Details)


toStyles : DefaultShapeTraits -> List Elm.Expression
toStyles node =
    HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait
        ++ HasGeometryTrait.toStyles node.hasGeometryTrait


toDetails : { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails node =
    Common.toDetails (toStyles node.defaultShapeTraits) node



toExpressions : Config -> String -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toExpressions config componentName node =
   Generate.Svg.DefaultShapeTraits.toExpressions config componentName node
{-
       let
           bbox =
               node.defaultShapeTraits.absoluteBoundingBox

           positionRelatively =
               case config.positionRelatively of
                   Just { x, y } ->
                       [ Attributes.css
                           [ "translate({{ x }}px, {{ y }}px)"
                               |> Format.namedValue "x" (bbox.x - x |> String.fromFloat)
                               |> Format.namedValue "y" (bbox.y - y |> String.fromFloat)
                               |> Css.property "transform"
                           ]
                       ]

                   Nothing ->
                       []
       in
       Generate.Svg.DefaultShapeTraits.toExpressions config componentName node
           |> Gen.Svg.Styled.svg
               (
                   ++ positionRelatively
               )
           |> List.singleton
-}
