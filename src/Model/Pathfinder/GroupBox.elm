module Model.Pathfinder.GroupBox exposing (Bounds, area, boundsFor, contains, headerHeight, nudgeOut, padding, pointFor)

{-| Geometry of the colored box drawn around a node group. Shared between the
renderer (`View.Pathfinder.Network`) and the drag-to-(un)group logic
(`Update.Pathfinder`) so both agree on where a group's box is.
-}

import Animation exposing (Animation, Clock)
import Config.Pathfinder
import Dict exposing (Dict)
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)
import Theme.Svg.GraphComponents as GraphComponents


type alias Bounds =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    }


{-| Padding (px) added around the outermost member nodes of a group box.
Must accommodate (1) the node body + the label rendered below, and (2) a
full snap-to-grid step — so a node dropped right next to a member, after
overlap-resolution pushes it outward by one grid step, still lands inside
the box and is captured as a group member.
-}
padding : Float
padding =
    max
        (GraphComponents.addressNodeNodeFrame_details.width / 2 + 56)
        (Config.Pathfinder.nodeYOffset * unit + 20)


{-| Height (px) of the group box's draggable header strip.
-}
headerHeight : Float
headerHeight =
    22


area : Bounds -> Float
area b =
    (b.maxX - b.minX) * (b.maxY - b.minY)


contains : Bounds -> ( Float, Float ) -> Bool
contains b ( x, y ) =
    x >= b.minX && x <= b.maxX && y >= b.minY && y <= b.maxY


{-| If the point lies inside one of the boxes, return a point moved just
outside that box's nearest edge. Nothing if the point is already clear of
every box.
-}
nudgeOut : List Bounds -> ( Float, Float ) -> Maybe ( Float, Float )
nudgeOut boxes ( x, y ) =
    boxes
        |> List.filter (\b -> contains b ( x, y ))
        |> List.head
        |> Maybe.map
            (\b ->
                let
                    margin =
                        8

                    dLeft =
                        x - b.minX

                    dRight =
                        b.maxX - x

                    dTop =
                        y - b.minY

                    dBottom =
                        b.maxY - y

                    least =
                        min (min dLeft dRight) (min dTop dBottom)
                in
                if least == dLeft then
                    ( b.minX - margin, y )

                else if least == dRight then
                    ( b.maxX + margin, y )

                else if least == dTop then
                    ( x, b.minY - margin )

                else
                    ( x, b.maxY + margin )
            )


{-| Center position of a node (address or tx) in px graph coordinates.
-}
pointFor : Dict Id Address -> Dict Id Tx -> Id -> Maybe ( Float, Float )
pointFor addresses txs id =
    case Dict.get id addresses of
        Just address ->
            Just (position address)

        Nothing ->
            Dict.get id txs |> Maybe.map position


{-| Padded bounding box (px) around the given member nodes. Nothing if none of
the ids resolve to a node currently on the graph.
-}
boundsFor : Dict Id Address -> Dict Id Tx -> List Id -> Maybe Bounds
boundsFor addresses txs ids =
    case List.filterMap (pointFor addresses txs) ids of
        first :: rest ->
            let
                xs =
                    List.map Tuple.first (first :: rest)

                ys =
                    List.map Tuple.second (first :: rest)
            in
            Just
                { minX = (List.minimum xs |> Maybe.withDefault 0) - padding
                , minY = (List.minimum ys |> Maybe.withDefault 0) - padding
                , maxX = (List.maximum xs |> Maybe.withDefault 0) + padding
                , maxY = (List.maximum ys |> Maybe.withDefault 0) + padding
                }

        [] ->
            Nothing


position : { a | x : Float, dx : Float, y : Animation, dy : Float, clock : Clock } -> ( Float, Float )
position node =
    ( (node.x + node.dx) * unit
    , (Animation.animate node.clock node.y + node.dy) * unit
    )
