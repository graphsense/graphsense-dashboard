module Model.Graph.Adding exposing (AddingAddress, AddingEntity, Model)

import Api.Data
import Dict exposing (Dict)
import Model.Graph.Id as Id
import Set exposing (Set)


type alias Model =
    { addresses : Dict ( String, String ) AddingAddress
    , entities : Dict ( String, Int ) AddingEntity
    , labels : Set String
    , addressPath : List Id.AddressId
    , entityPath : List Id.EntityId
    }


type alias AddingAddress =
    { address : Maybe Api.Data.Address
    , entity : Maybe Api.Data.Cluster
    , outgoing : Maybe (List Api.Data.NeighborCluster)
    , incoming : Maybe (List Api.Data.NeighborCluster)
    , anchor : Maybe ( Bool, Id.AddressId )
    }


type alias AddingEntity =
    { entity : Maybe Api.Data.Cluster
    , outgoing : Maybe (List Api.Data.NeighborCluster)
    , incoming : Maybe (List Api.Data.NeighborCluster)
    }
