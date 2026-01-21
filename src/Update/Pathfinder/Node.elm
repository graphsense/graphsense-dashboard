module Update.Pathfinder.Node exposing (Node, move, moveAbs, release, setX, setY)

import Animation exposing (Animation, Clock)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)


type alias Node a =
    { a
        | dx : Float
        , dy : Float
        , x : Float
        , y : Animation
        , clock : Clock
        , opacity : Animation
        , id : Id
    }


move : Coords -> Node a -> Node a
move vector node =
    { node
        | dx = vector.x
        , dy = vector.y
    }


moveAbs : Coords -> Node a -> Node a
moveAbs position node =
    { node
        | x = position.x
        , y = node.y |> Animation.to position.y
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


setX : Float -> Node a -> Node a
setX newX node =
    { node
        | x = newX
    }


setY : Float -> Node a -> Node a
setY newY node =
    { node
        | y = Animation.static newY
    }
