module Generate.Common.VectorNode exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import RecordSetter exposing (s_cornerRadiusShapeTraits)
import Types exposing (OriginAdjust)
import Dict exposing (Dict)


getName : VectorNode -> String
getName node =
    DefaultShapeTraits.getName node.cornerRadiusShapeTraits


adjustBoundingBox : OriginAdjust -> VectorNode -> VectorNode
adjustBoundingBox adjust node =
    node.cornerRadiusShapeTraits
        |> DefaultShapeTraits.adjustBoundingBox adjust
        |> flip s_cornerRadiusShapeTraits node

adjustName : Dict String String -> VectorNode -> VectorNode
adjustName names node =
    node.cornerRadiusShapeTraits
        |> DefaultShapeTraits.adjustName names
        |> flip s_cornerRadiusShapeTraits node
