module Model.Pathfinder.Address exposing (..)

import Set exposing (Set)


type alias Address =
    { x : Float
    , y : Float
    , id : String
    , transactions : Set String
    }
