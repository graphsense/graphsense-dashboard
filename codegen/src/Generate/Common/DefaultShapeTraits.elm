module Generate.Common.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Elm
import Generate.Util exposing (..)
import RecordSetter exposing (s_absoluteBoundingBox, s_defaultShapeTraits)
import Types exposing (Details, OriginAdjust)


adjustBoundingBox : OriginAdjust -> { a | defaultShapeTraits : DefaultShapeTraits } -> { a | defaultShapeTraits : DefaultShapeTraits }
adjustBoundingBox { x, y } node =
    node.defaultShapeTraits.absoluteBoundingBox
        |> (\bb -> { bb | x = bb.x - x, y = bb.y - y })
        |> flip s_absoluteBoundingBox node.defaultShapeTraits
        |> flip s_defaultShapeTraits node


getName : { a | defaultShapeTraits : DefaultShapeTraits } -> String
getName node =
    node.defaultShapeTraits.isLayerTrait.name


getBoundingBox : { a | defaultShapeTraits : DefaultShapeTraits } -> Rectangle
getBoundingBox node =
    node.defaultShapeTraits.absoluteBoundingBox


getStrokeWidth : { a | defaultShapeTraits : DefaultShapeTraits } -> Float
getStrokeWidth node =
    node.defaultShapeTraits.strokeWeight
        |> Maybe.withDefault 0


toDetails : List Elm.Expression -> { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails styles node =
    { name = getName node
    , bbox = getBoundingBox node
    , strokeWidth = getStrokeWidth node
    , styles = styles
    }
