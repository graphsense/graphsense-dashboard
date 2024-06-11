module Update.Pathfinder.Address exposing (move, release)

import Animation
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Address exposing (..)


move : Coords -> Address -> Address
move vector address =
    { address
        | dx = vector.x
        , dy = vector.y
    }


release : Address -> Address
release address =
    { address
        | x = address.x + address.dx
        , y =
            Animation.getTo address.y
                + address.dy
                |> Animation.static
        , dx = 0
        , dy = 0
    }
