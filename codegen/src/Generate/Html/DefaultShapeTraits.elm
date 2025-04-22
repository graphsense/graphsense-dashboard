module Generate.Html.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Css as Css
import Gen.Html.Styled
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common exposing (wrapInSvg)
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Html.HasEffectsTrait as HasEffectsTrait
import Generate.Html.HasGeometryTrait as HasGeometryTrait
import Generate.Util exposing (..)
import Types exposing (ColorMap, Config, Details)


toStyles : ColorMap -> DefaultShapeTraits -> List Elm.Expression
toStyles colorMap node =
    HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait
        ++ HasGeometryTrait.toStyles colorMap node.hasGeometryTrait Nothing
        ++ HasEffectsTrait.toStyles colorMap node.hasEffectsTrait


toExpressions : Config -> DefaultShapeTraits -> List Elm.Expression -> List Elm.Expression
toExpressions config node =
    wrapInSvg config (Common.getName node) node
        >> List.singleton



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
