module Model.Pathfinder.Tx exposing (..)

import Dict exposing (Dict)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Input exposing (Input)
import Model.Pathfinder.Output exposing (Output)


type alias Tx =
    { inputs : Dict Id Input
    , outputs : Dict Id Output
    }
