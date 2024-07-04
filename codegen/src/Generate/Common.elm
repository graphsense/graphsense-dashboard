module Generate.Common exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip, uncurry)
import Elm
import Generate.Common.DefaultShapeTraits as DefaultShapeTraits
import Generate.Common.FrameTraits as FrameTraits
import Generate.Common.RectangleNode as RectangleNode
import Generate.Common.VectorNode as VectorNode
import RecordSetter exposing (s_children, s_frameTraits)
import Types exposing (Details, OriginAdjust)


adjustBoundingBoxes : ComponentNode -> ComponentNode
adjustBoundingBoxes node =
    let
        originAdjust =
            getOriginAdjust node
    in
    withFrameTraitsAdjustBoundingBox originAdjust node


subcanvasNodeAdjustBoundingBox : OriginAdjust -> SubcanvasNode -> SubcanvasNode
subcanvasNodeAdjustBoundingBox adjust node =
    case node of
        SubcanvasNodeTextNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
                |> SubcanvasNodeTextNode

        SubcanvasNodeEllipseNode n ->
            DefaultShapeTraits.adjustBoundingBox adjust n
                |> SubcanvasNodeEllipseNode

        SubcanvasNodeGroupNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeGroupNode

        SubcanvasNodeInstanceNode n ->
            withFrameTraitsAdjustBoundingBox adjust n
                |> SubcanvasNodeInstanceNode

        SubcanvasNodeVectorNode n ->
            VectorNode.adjustBoundingBox adjust n
                |> SubcanvasNodeVectorNode

        SubcanvasNodeRectangleNode n ->
            RectangleNode.adjustBoundingBox adjust n
                |> SubcanvasNodeRectangleNode

        a ->
            a


withFrameTraitsAdjustBoundingBox : OriginAdjust -> { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
withFrameTraitsAdjustBoundingBox adjust node =
    node.frameTraits.children
        |> List.map (subcanvasNodeAdjustBoundingBox adjust)
        |> flip s_children node.frameTraits
        |> flip s_frameTraits node
        |> FrameTraits.adjustBoundingBox adjust


getOriginAdjust : ComponentNode -> OriginAdjust
getOriginAdjust node =
    node.frameTraits.absoluteBoundingBox
        |> (\r ->
                { x = r.x
                , y = r.y
                }
           )

