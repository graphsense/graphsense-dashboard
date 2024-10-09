module Generate.Svg.VectorNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Css as Css
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Svg.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import List.Nonempty as NList
import Types exposing (ColorMap, Config, Details)


toExpressions : Config -> String -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toExpressions config componentName node =
    if Common.isHidden node then
        []

    else
        Gen.Svg.Styled.g
            (toAttributes node)
            [ toStrokePaths config node
            , toFillPaths config node
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
        css =
            node.defaultShapeTraits.hasGeometryTrait.minimalStrokesTrait
                |> strokeStyles config.colorMap
    in
    node.defaultShapeTraits.strokeGeometry
        |> Maybe.andThen NList.fromList
        |> Maybe.map NList.toList
        |> Maybe.map
            (List.map renderPath
                >> Elm.list
                >> Gen.Svg.Styled.call_.g
                    (Common.getName node
                        |> getElementAttributes config
                        |> Elm.Op.append
                            ([ css |> Attributes.css ]
                                |> Elm.list
                            )
                    )
            )
        |> Maybe.withDefault (Gen.Svg.Styled.g [] [])


toFillPaths : Config -> { a | defaultShapeTraits : DefaultShapeTraits } -> Elm.Expression
toFillPaths config node =
    let
        css =
            node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait
                |> MinimalFillsTrait.toStyles config.colorMap
    in
    node.defaultShapeTraits.fillGeometry
        |> Maybe.map (List.map renderPath)
        |> Maybe.withDefault []
        |> Elm.list
        |> Gen.Svg.Styled.call_.g
            (Common.getName node
                |> getElementAttributes config
                |> Elm.Op.append
                    ([ css |> Attributes.css ]
                        |> Elm.list
                    )
            )


toStyles : ColorMap -> { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toStyles colorMap node =
    MinimalFillsTrait.toStyles colorMap node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait
        ++ strokeStyles colorMap node.defaultShapeTraits.hasGeometryTrait.minimalStrokesTrait


strokeStyles : ColorMap -> MinimalStrokesTrait -> List Elm.Expression
strokeStyles colorMap node =
    -- same as MiniStrokesTrait but putting strokes coler into fill
    []
        |> m (Paint.toStylesString colorMap >> Maybe.withDefault "transparent" >> Css.property "fill") node.strokes
        |> a MinimalFillsTrait.opacity node.strokes


toDetails : ColorMap -> { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails colorMap node =
    Common.toDetails (toStyles colorMap node) node


toAttributes : { a | defaultShapeTraits : DefaultShapeTraits } -> List Elm.Expression
toAttributes node =
    [ toTranslate node.defaultShapeTraits.absoluteBoundingBox
        |> Attributes.transform
    ]
