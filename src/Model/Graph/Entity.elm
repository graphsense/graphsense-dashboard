module Model.Graph.Entity exposing (..)

import Api.Data
import Model.Graph.Address exposing (..)
import Model.Graph.Id exposing (..)


type alias Entity =
    { id : EntityId
    , entity : Api.Data.Entity
    , addresses : List Address
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }
