module Generate.Svg.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Basics.Extra exposing (flip)
import Elm
import Generate.Svg.CornerTrait as CornerTrait
import Generate.Svg.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import RecordSetter exposing (s_absoluteBoundingBox, s_frameTraits)
import Types exposing (Metadata, OriginAdjust)


toCss : FrameTraits -> List Elm.Expression
toCss node =
    CornerTrait.toCss node.cornerTrait
        ++ HasBlendModeAndOpacityTrait.toCss node.hasBlendModeAndOpacityTrait


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
