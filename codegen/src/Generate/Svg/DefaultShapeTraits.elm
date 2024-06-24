module Generate.Svg.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Elm
import Generate.Svg.HasGeometryTrait as HasGeometryTrait
import Generate.Util exposing (..)
import RecordSetter exposing (s_absoluteBoundingBox, s_defaultShapeTraits)
import Types exposing (Metadata, OriginAdjust)


toCss : DefaultShapeTraits -> List Elm.Expression
toCss node =
    HasGeometryTrait.toCss node.hasGeometryTrait


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
    }
