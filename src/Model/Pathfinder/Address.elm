module Model.Pathfinder.Address exposing (..)

import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (WebData)
import Set exposing (Set)


type alias Address =
    { x : Float
    , y : Float
    , id : Id
    , transactions : WebData (Set String)
    }
