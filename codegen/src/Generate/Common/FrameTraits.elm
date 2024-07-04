module Generate.Common.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits, Rectangle)
import Basics.Extra exposing (flip)
import Elm
import RecordSetter exposing (s_absoluteBoundingBox, s_frameTraits)
import Types exposing (Details, OriginAdjust)


adjustBoundingBox : OriginAdjust -> { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
adjustBoundingBox { x, y } node =
    node.frameTraits.absoluteBoundingBox
        |> (\bb -> { bb | x = bb.x - x, y = bb.y - y })
        |> flip s_absoluteBoundingBox node.frameTraits
        |> flip s_frameTraits node


getName : { a | frameTraits : FrameTraits } -> String
getName node =
    node.frameTraits.isLayerTrait.name


getBoundingBox : { a | frameTraits : FrameTraits } -> Rectangle
getBoundingBox node =
    node.frameTraits.absoluteBoundingBox


getStrokeWidth : { a | frameTraits : FrameTraits } -> Float
getStrokeWidth node =
    node.frameTraits.strokeWeight
        |> Maybe.withDefault 0


toDetails : (FrameTraits -> List Elm.Expression) -> { a | frameTraits : FrameTraits } -> Details
toDetails getStyles node =
    { name = getName node
    , bbox = getBoundingBox node
    , strokeWidth = getStrokeWidth node
    , styles = getStyles node.frameTraits
    }
