module Model.Graph.Adding exposing (..)

import Api.Data
import Dict exposing (Dict)
import RemoteData exposing (WebData)
import Set exposing (Set)


type alias Model =
    { addresses : Dict ( String, String ) AddingAddress
    , entities : Dict ( String, Int ) AddingEntity
    , labels : Set String
    }


type alias AddingAddress =
    { address : Maybe Api.Data.Address
    , entity : Maybe Api.Data.Entity
    , outgoing : Maybe (List Api.Data.NeighborEntity)
    , incoming : Maybe (List Api.Data.NeighborEntity)
    }


type alias AddingEntity =
    { entity : Maybe Api.Data.Entity
    , outgoing : Maybe (List Api.Data.NeighborEntity)
    , incoming : Maybe (List Api.Data.NeighborEntity)
    }
