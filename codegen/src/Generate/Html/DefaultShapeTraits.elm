module Generate.Html.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Css as Css
import Gen.Html.Styled
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Html.HasEffectsTrait as HasEffectsTrait
import Generate.Html.HasGeometryTrait as HasGeometryTrait
import Generate.Util exposing (..)
import Types exposing (ColorMap, Config, Details)


toStyles : ColorMap -> DefaultShapeTraits -> List Elm.Expression
toStyles colorMap node =
    HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait
        ++ HasGeometryTrait.toStyles colorMap node.hasGeometryTrait
        ++ HasEffectsTrait.toStyles colorMap node.hasEffectsTrait


toDetails : ColorMap -> { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails colorMap node =
    Common.toDetails (toStyles colorMap node.defaultShapeTraits) node


toExpressions : Config -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression -> List Elm.Expression
toExpressions config node children =
    let
        name =
            node.defaultShapeTraits.isLayerTrait.name

        toHtml =
            let
                bbox =
                    node.defaultShapeTraits.absoluteBoundingBox

                positionRelatively =
                    Common.positionRelatively config node.defaultShapeTraits
            in
            Gen.Svg.Styled.call_.svg
                ([ max 3 bbox.width
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
                    |> Elm.list
                )
                >> List.singleton
                >> Elm.list
                >> Gen.Html.Styled.call_.div
                    (getElementAttributes config name
                        |> Elm.Op.append
                            (positionRelatively
                                |> Attributes.css
                                |> List.singleton
                                |> Elm.list
                            )
                    )
    in
    children
        |> Elm.list
        |> toHtml
        |> List.singleton



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
