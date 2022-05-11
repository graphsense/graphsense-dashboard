module Update.Graph.Address exposing (move, release)

import Model.Graph.Address exposing (..)
import Model.Graph.Coords exposing (Coords)


move : Coords -> Address -> Address
move { x, y } address =
    { address
        | dx = x
        , dy = y
    }


release : Address -> Address
release address =
    { address
        | dx = 0
        , dy = 0
        , x = address.x + address.dx
        , y = address.y + address.dy
    }
