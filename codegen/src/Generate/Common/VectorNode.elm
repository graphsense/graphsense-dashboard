module Generate.Common.VectorNode exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import RecordSetter exposing (s_cornerRadiusShapeTraits)
import Types exposing (OriginAdjust)


getName : VectorNode -> String
getName node =
    node.cornerRadiusShapeTraits.defaultShapeTraits.isLayerTrait.name


adjustBoundingBox : OriginAdjust -> VectorNode -> VectorNode
adjustBoundingBox adjust node =
    node.cornerRadiusShapeTraits
        |> DefaultShapeTraits.adjustBoundingBox adjust
        |> flip s_cornerRadiusShapeTraits node
