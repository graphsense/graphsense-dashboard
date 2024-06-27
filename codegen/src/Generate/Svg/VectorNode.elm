module Generate.Svg.VectorNode exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Elm
import Gen.Css as Css
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Svg.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (a, toMatrix, toRotate, toTranslate, withVisibility)
import Generate.Util.Paint as Paint
import RecordSetter exposing (s_cornerRadiusShapeTraits)
import Tuple exposing (pair)
import Types exposing (Config, OriginAdjust)


toExpressions : Config -> VectorNode -> List Elm.Expression
toExpressions config node =
    Gen.Svg.Styled.g
        (toAttributes node)
        [ toStrokePaths node
        , toFillPaths node
        ]
        |> withVisibility config.propertyExpressions node.cornerRadiusShapeTraits.defaultShapeTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


getName : VectorNode -> String
getName node =
    node.cornerRadiusShapeTraits.defaultShapeTraits.isLayerTrait.name


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
                    (Paint.toCssString
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
                |> MinimalFillsTrait.toCss
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
        --++ " "
        --++ (Maybe.map toMatrix node.cornerRadiusShapeTraits.defaultShapeTraits.relativeTransform |> Maybe.withDefault "")
        |> Attributes.transform
    ]


adjustBoundingBox : OriginAdjust -> VectorNode -> VectorNode
adjustBoundingBox adjust node =
    node.cornerRadiusShapeTraits
        |> DefaultShapeTraits.adjustBoundingBox adjust
        |> flip s_cornerRadiusShapeTraits node
