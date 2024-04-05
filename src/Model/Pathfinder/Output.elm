module Model.Pathfinder.Output exposing (..)

import Api.Data
import Model.Pathfinder.Id exposing (Id)


type alias Output =
    { address : Id
    , value : Api.Data.Values
    }
