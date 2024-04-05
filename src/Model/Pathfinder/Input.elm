module Model.Pathfinder.Input exposing (..)

import Api.Data
import Model.Pathfinder.Id exposing (Id)


type alias Input =
    { address : Id
    , value : Api.Data.Values
    }
