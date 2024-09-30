module Generate.Svg.RectangleNode exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Elm exposing (Expression)
import Elm.Op
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes exposing (height, width, x, y)
import Generate.Common.DefaultShapeTraits as Common
import Generate.Common.RectangleNode exposing (getName)
import Generate.Svg.DefaultShapeTraits as DefaultShapeTraits
import Generate.Util exposing (getElementAttributes, withVisibility)
import RecordSetter exposing (..)
import Types exposing (Config, Details, OriginAdjust)
import Generate.Util exposing (mm2)
import Generate.Svg.CornerTrait as CornerTrait


toExpressions : Config -> String -> RectangleNode -> List Elm.Expression
toExpressions config componentName node =
    Gen.Svg.Styled.call_.rect
        (getName node
            |> getElementAttributes config
            |> Elm.Op.append
                ((toStyles node
                    |> Gen.Svg.Styled.Attributes.css
                 )
                    :: toAttributes node
                    |> Elm.list
                )
        )
        (Elm.list [])
        |> withVisibility componentName config.propertyExpressions node.rectangularShapeTraits.defaultShapeTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


toStyles : RectangleNode -> List Elm.Expression
toStyles node =
    DefaultShapeTraits.toStyles node.rectangularShapeTraits.defaultShapeTraits


toDetails : RectangleNode -> Details
toDetails node =
    Common.toDetails (toStyles node) node.rectangularShapeTraits


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
    node.rectangularShapeTraits
        |> Common.adjustBoundingBox adjust
        |> flip s_rectangularShapeTraits node
