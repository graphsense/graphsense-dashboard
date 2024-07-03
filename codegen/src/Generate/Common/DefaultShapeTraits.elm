module Generate.Common.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Color exposing (Color)
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import RecordSetter exposing (s_absoluteBoundingBox, s_defaultShapeTraits)
import Types exposing (Metadata, OriginAdjust)


adjustBoundingBox : OriginAdjust -> { a | defaultShapeTraits : DefaultShapeTraits } -> { a | defaultShapeTraits : DefaultShapeTraits }
adjustBoundingBox { x, y } node =
    node.defaultShapeTraits.absoluteBoundingBox
        |> (\bb -> { bb | x = bb.x - x, y = bb.y - y })
        |> flip s_absoluteBoundingBox node.defaultShapeTraits
        |> flip s_defaultShapeTraits node


toMetadata : { a | defaultShapeTraits : DefaultShapeTraits } -> Metadata
toMetadata node =
    { name = node.defaultShapeTraits.isLayerTrait.name
    , bbox = node.defaultShapeTraits.absoluteBoundingBox
    , strokeWidth =
        node.defaultShapeTraits.strokeWeight
            |> Maybe.withDefault 0
    , strokeColor = Paint.toColor node.defaultShapeTraits.hasGeometryTrait.strokes
    , fillColor = Paint.toColor <| Just node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait.fills
    , strokeOpacity =
        node.defaultShapeTraits.hasGeometryTrait.strokes
            |> Paint.getOpacity
    , fillOpacity =
        node.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait.fills
        |> Just
        |> Paint.getOpacity
    }
