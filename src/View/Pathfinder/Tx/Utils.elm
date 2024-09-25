module View.Pathfinder.Tx.Utils exposing (AnimatedPosTrait, signX, toPosition, Pos)

import Animation exposing (Animation, Clock)


type alias AnimatedPosTrait ex =
    { ex | x : Float, dx : Float, y : Animation, dy : Float, opacity : Animation, clock : Clock }


type alias Pos =
    { x : Float, y : Float }


toPosition : AnimatedPosTrait x -> Pos
toPosition thing =
    { x = thing.x + thing.dx
    , y = Animation.animate thing.clock thing.y + thing.dy
    }


signX : Pos -> Pos -> Float
signX f t =
    if f.x > t.x then
        -1

    else
        1
