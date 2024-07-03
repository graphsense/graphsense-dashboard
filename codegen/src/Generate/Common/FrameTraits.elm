module Generate.Common.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Basics.Extra exposing (flip)
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
    }
