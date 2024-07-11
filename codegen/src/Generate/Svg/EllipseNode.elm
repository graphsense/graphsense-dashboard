module Generate.Svg.EllipseNode exposing (..)

import Api.Raw exposing (..)
import Elm exposing (Expression)
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes exposing (cx, cy, rx, ry)
import Generate.Common.DefaultShapeTraits as Common
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Util exposing (getElementAttributes, withVisibility)
import RecordSetter exposing (..)
import Types exposing (Config)


toExpressions : Config -> ( String, String ) -> EllipseNode -> List Elm.Expression
toExpressions config componentNameId node =
    let
        name =
            getName node
    in
    Gen.Svg.Styled.call_.ellipse
        (getElementAttributes config name
            |> Elm.Op.append
                ((toStyles node
                    |> Gen.Svg.Styled.Attributes.css
                 )
                    :: toAttributes node
                    |> Elm.list
                )
        )
        (Elm.list [])
        |> withVisibility componentNameId config.propertyExpressions node.defaultShapeTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


getName : EllipseNode -> String
getName node =
    node.defaultShapeTraits.isLayerTrait.name


toStyles : EllipseNode -> List Elm.Expression
toStyles node =
    DefaultShapeTraits.toStyles node.defaultShapeTraits


toAttributes : EllipseNode -> List Elm.Expression
toAttributes node =
    toSize node.defaultShapeTraits
        ++ toCoords node.defaultShapeTraits.absoluteBoundingBox


toCoords : Rectangle -> List Elm.Expression
toCoords b =
    [ b.x + b.width / 2 |> String.fromFloat |> cx
    , b.y + b.height / 2 |> String.fromFloat |> cy
    ]


toSize : DefaultShapeTraits -> List Expression
toSize shape =
    let
        strokeWeight =
            if Maybe.map List.isEmpty shape.hasGeometryTrait.strokes |> Maybe.withDefault True then
                0

            else
                shape.hasGeometryTrait.strokeWeight
                    |> Maybe.withDefault 0

        adjustStrokeAlign =
            case shape.hasGeometryTrait.strokeAlign of
                Just StrokeAlignINSIDE ->
                    -strokeWeight / 2

                Just StrokeAlignCENTER ->
                    0

                Just StrokeAlignOUTSIDE ->
                    strokeWeight / 2

                Nothing ->
                    0
    in
    [ shape.absoluteBoundingBox.width / 2 + adjustStrokeAlign |> String.fromFloat |> rx
    , shape.absoluteBoundingBox.height / 2 + adjustStrokeAlign |> String.fromFloat |> ry
    ]
