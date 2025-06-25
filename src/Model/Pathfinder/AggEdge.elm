module Model.Pathfinder.AggEdge exposing (AggEdge)

import Api.Data
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (WebData)
import Set exposing (Set)


type alias AggEdge =
    { a : Id
    , b : Id
    , aAddress : Maybe Address
    , bAddress : Maybe Address
    , a2b : WebData (Maybe Api.Data.NeighborAddress)
    , b2a : WebData (Maybe Api.Data.NeighborAddress)
    , txs : Set Id
    , selected : Bool
    , alwaysShow : Bool
    }
