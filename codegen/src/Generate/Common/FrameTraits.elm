module Generate.Common.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Basics.Extra exposing (flip)
import Generate.Util.Paint as Paint
import RecordSetter exposing (s_absoluteBoundingBox, s_frameTraits)
import Types exposing (Metadata, OriginAdjust)


adjustBoundingBox : OriginAdjust -> { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
adjustBoundingBox { x, y } node =
    node.frameTraits.absoluteBoundingBox
        |> (\bb -> { bb | x = bb.x - x, y = bb.y - y })
        |> flip s_absoluteBoundingBox node.frameTraits
        |> flip s_frameTraits node


toMetadata : { a | frameTraits : FrameTraits } -> Metadata
toMetadata node =
    { name = node.frameTraits.isLayerTrait.name
    , bbox = node.frameTraits.absoluteBoundingBox
    , strokeWidth =
        node.frameTraits.strokeWeight
            |> Maybe.withDefault 0
    , strokeColor = Paint.toColor node.frameTraits.strokes
    , fillColor = Paint.toColor <| Just node.frameTraits.fills
    , strokeOpacity = Paint.getOpacity node.frameTraits.strokes
    , fillOpacity = Paint.getOpacity <| Just node.frameTraits.fills
    }
