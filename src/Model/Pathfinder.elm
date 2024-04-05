module Model.Pathfinder exposing (..)

import Model.Pathfinder.Network exposing (Network)


type alias Model =
    { networks : List Network
    }
