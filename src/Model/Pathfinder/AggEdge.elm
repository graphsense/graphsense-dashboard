module Model.Pathfinder.AggEdge exposing (AggEdge)

import Animation exposing (Animation, Clock)
import Api.Data
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Set exposing (Set)


type alias AggEdge =
    { id : ( Id, Id )
    , fromAddress : Maybe Address
    , toAddress : Maybe Address
    , fromNeighborData : Maybe Api.Data.NeighborAddress
    , toNeighborData : Maybe Api.Data.NeighborAddress
    , txs : Set Id
    }
