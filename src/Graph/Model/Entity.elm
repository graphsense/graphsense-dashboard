module Graph.Model.Entity exposing (..)

import Api.Data
import Graph.Model.Address exposing (..)
import Graph.Model.Id exposing (..)


type alias Entity =
    { id : EntityId
    , entity : Api.Data.Entity
    , addresses : List Address
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }
