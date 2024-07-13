module Generate.Common.RectangleNode exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import RecordSetter exposing (..)
import Types exposing (OriginAdjust)


getName : RectangleNode -> String
getName node =
    DefaultShapeTraits.getName node.rectangularShapeTraits


adjustBoundingBox : OriginAdjust -> RectangleNode -> RectangleNode
adjustBoundingBox adjust node =
    node.rectangularShapeTraits
        |> DefaultShapeTraits.adjustBoundingBox adjust
        |> flip s_rectangularShapeTraits node


adjustName : Dict String String -> RectangleNode -> RectangleNode
adjustName names node =
    node.rectangularShapeTraits
        |> DefaultShapeTraits.adjustName names
        |> flip s_rectangularShapeTraits node
