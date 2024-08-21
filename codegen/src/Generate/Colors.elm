module Generate.Colors exposing (..)

import Api.Raw exposing (FrameNode, SubcanvasNode(..))
import Elm
import Generate.Common.RectangleNode as RectangleNode
import Generate.Util exposing (sanitize)
import Generate.Util.Paint as Paint


frameNodeToDeclarations : FrameNode -> List Elm.Declaration
frameNodeToDeclarations node =
    node.frameTraits.children
        |> List.filterMap
            (\c ->
                case c of
                    SubcanvasNodeRectangleNode n ->
                        Just n

                    _ ->
                        Nothing
            )
        |> List.filterMap
            (\r ->
                Paint.toColor r.rectangularShapeTraits.defaultShapeTraits.hasGeometryTrait.minimalFillsTrait.fills
                    |> Maybe.map
                        (Elm.declaration (RectangleNode.getName r |> sanitize))
            )
