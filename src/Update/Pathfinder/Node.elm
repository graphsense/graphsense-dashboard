module Update.Pathfinder.Node exposing (move, release)

import Animation exposing (Animation)
import Model.Graph.Coords exposing (Coords)


type alias Node a =
    { a
        | dx : Float
        , dy : Float
        , x : Float
        , y : Animation
    }


move : Coords -> Node a -> Node a
move vector node =
    { node
        | dx = vector.x
        , dy = vector.y
    }


release : Node a -> Node a
release node =
    { node
        | x = node.x + node.dx
        , y =
            Animation.getTo node.y
                + node.dy
                |> Animation.static
        , dx = 0
        , dy = 0
    }
