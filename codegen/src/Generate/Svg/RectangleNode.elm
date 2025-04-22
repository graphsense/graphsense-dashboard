module Generate.Svg.RectangleNode exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Elm exposing (Expression)
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes exposing (height, width, x, y)
import Generate.Common.DefaultShapeTraits as Common
import Generate.Common.RectangleNode exposing (getName)
import Generate.Svg.CornerTrait as CornerTrait
import Generate.Svg.HasGeometryTrait as HasGeometryTrait
import Generate.Util exposing (callStyles, getElementAttributes, withVisibility)
import RecordSetter exposing (..)
import Types exposing (ColorMap, Config, Details, OriginAdjust)


toExpressions : Config -> String -> RectangleNode -> List Elm.Expression
toExpressions config componentName node =
    let
        name =
            getName node
    in
    Gen.Svg.Styled.call_.rect
        (name
            |> getElementAttributes config
            |> Elm.Op.append
                ((callStyles config name
                    |> Gen.Svg.Styled.Attributes.call_.css
                 )
                    :: toAttributes node
                    |> Elm.list
                )
        )
        (Elm.list [])
        |> withVisibility componentName config.propertyExpressions node.rectangularShapeTraits.defaultShapeTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


toStyles : ColorMap -> RectangleNode -> List Elm.Expression
toStyles colorMap node =
    HasGeometryTrait.toStyles colorMap node.rectangularShapeTraits.defaultShapeTraits.hasGeometryTrait


toAttributes : RectangleNode -> List Elm.Expression
toAttributes node =
    toSize node.rectangularShapeTraits.defaultShapeTraits
        ++ toCoords node.rectangularShapeTraits.defaultShapeTraits.absoluteBoundingBox
        ++ CornerTrait.toAttributes node.rectangularShapeTraits.cornerTrait


toCoords : Rectangle -> List Elm.Expression
toCoords b =
    [ b.x |> String.fromFloat |> x
    , b.y |> String.fromFloat |> y
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
    [ shape.absoluteBoundingBox.width + adjustStrokeAlign |> String.fromFloat |> width
    , shape.absoluteBoundingBox.height + adjustStrokeAlign |> String.fromFloat |> height
    ]


adjustBoundingBox : OriginAdjust -> RectangleNode -> RectangleNode
adjustBoundingBox adjust node =
    node.rectangularShapeTraits.defaultShapeTraits
        |> Common.adjustBoundingBox adjust
        |> flip s_defaultShapeTraits node.rectangularShapeTraits
        |> flip s_rectangularShapeTraits node
