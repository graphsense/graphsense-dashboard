module Generate.Svg.LineNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Svg.HasGeometryTrait as HasGeometryTrait
import Generate.Util exposing (..)
import List.Nonempty as NList
import Types exposing (ColorMap, Config)


toExpressions : Config -> String -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toExpressions config componentName node =
    if Common.isHidden node then
        []

    else
        Gen.Svg.Styled.g
            (toAttributes node)
            [ toStrokePaths config node
            ]
            |> withVisibility componentName config.propertyExpressions node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
            |> List.singleton


renderPath : { a | path : String } -> Elm.Expression
renderPath { path } =
    Gen.Svg.Styled.path
        [ Attributes.d path
        ]
        []


toStrokePaths : Config -> { a | defaultShapeTraits : DefaultShapeTraits } -> Elm.Expression
toStrokePaths config node =
    let
        name =
            Common.getName node.defaultShapeTraits
    in
    node.defaultShapeTraits.strokeGeometry
        |> Maybe.andThen NList.fromList
        |> Maybe.map NList.toList
        |> Maybe.map
            (List.map renderPath
                >> Elm.list
                >> Gen.Svg.Styled.call_.g
                    (name
                        |> getElementAttributes config
                        |> Elm.Op.append
                            (callStyles config name
                                |> Attributes.call_.css
                                |> List.singleton
                                |> Elm.list
                            )
                    )
            )
        |> Maybe.withDefault (Gen.Svg.Styled.g [] [])


toStyles : ColorMap -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toStyles colorMap node =
    HasGeometryTrait.toStyles colorMap node.defaultShapeTraits.hasGeometryTrait


toAttributes : { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toAttributes node =
    [ toTranslate node.defaultShapeTraits.absoluteBoundingBox
        |> Attributes.transform
    ]
