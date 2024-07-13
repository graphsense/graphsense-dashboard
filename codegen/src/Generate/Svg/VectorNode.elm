module Generate.Svg.VectorNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (a, toTranslate, withVisibility)
import Generate.Util.Paint as Paint
import Types exposing (Config, Details)


toExpressions : Config -> String -> VectorNode -> List Elm.Expression
toExpressions config componentName node =
    Gen.Svg.Styled.g
        (toAttributes node)
        [ toStrokePaths node
        , toFillPaths node
        ]
        |> withVisibility componentName config.propertyExpressions node.cornerRadiusShapeTraits.defaultShapeTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


renderPath : { a | path : String } -> Elm.Expression
renderPath { path } =
    Gen.Svg.Styled.path
        [ Attributes.d path
        ]
        []


toStrokePaths : VectorNode -> Elm.Expression
toStrokePaths node =
    let
        strokes =
            node.cornerRadiusShapeTraits.defaultShapeTraits.hasGeometryTrait.strokes

        css =
            []
                |> a
                    (Paint.toStylesString
                        >> Maybe.map (Css.call_.property (Elm.string "fill"))
                    )
                    strokes
                |> a
                    (Paint.getBasePaint
                        >> Maybe.andThen .opacity
                        >> Maybe.map String.fromFloat
                        >> Maybe.map (Css.property "opacity")
                    )
                    strokes
    in
    node.cornerRadiusShapeTraits.defaultShapeTraits.strokeGeometry
        |> Maybe.map (List.map renderPath)
        |> Maybe.withDefault []
        |> Elm.list
        |> Gen.Svg.Styled.call_.g
            ([ css |> Attributes.css ]
                |> Elm.list
            )


toFillPaths : VectorNode -> Elm.Expression
toFillPaths node =
    let
        css =
            node.cornerRadiusShapeTraits.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait
                |> MinimalFillsTrait.toStyles
    in
    node.cornerRadiusShapeTraits.defaultShapeTraits.fillGeometry
        |> Maybe.map (List.map renderPath)
        |> Maybe.withDefault []
        |> Elm.list
        |> Gen.Svg.Styled.call_.g
            ([ css |> Attributes.css ]
                |> Elm.list
            )


toAttributes : VectorNode -> List Elm.Expression
toAttributes node =
    [ toTranslate node.cornerRadiusShapeTraits.defaultShapeTraits.absoluteBoundingBox
        |> Attributes.transform
    ]


toDetails : VectorNode -> Details
toDetails node =
    Common.toDetails (DefaultShapeTraits.toStyles node.cornerRadiusShapeTraits.defaultShapeTraits) node.cornerRadiusShapeTraits
