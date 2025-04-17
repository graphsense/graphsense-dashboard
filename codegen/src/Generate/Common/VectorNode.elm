module Generate.Common.VectorNode exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import RecordSetter exposing (s_cornerRadiusShapeTraits, s_defaultShapeTraits)
import Types exposing (OriginAdjust)


getName : VectorNode -> String
getName node =
    DefaultShapeTraits.getName node.cornerRadiusShapeTraits.defaultShapeTraits


adjustBoundingBox : OriginAdjust -> VectorNode -> VectorNode
adjustBoundingBox adjust node =
    node.cornerRadiusShapeTraits.defaultShapeTraits
        |> DefaultShapeTraits.adjustBoundingBox adjust
        |> flip s_defaultShapeTraits node.cornerRadiusShapeTraits
        |> flip s_cornerRadiusShapeTraits node


adjustName : Dict String String -> VectorNode -> VectorNode
adjustName names node =
    node.cornerRadiusShapeTraits.defaultShapeTraits
        |> DefaultShapeTraits.adjustName names
        |> flip s_defaultShapeTraits node.cornerRadiusShapeTraits
        |> flip s_cornerRadiusShapeTraits node
