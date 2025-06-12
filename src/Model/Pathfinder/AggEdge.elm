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

    -- the relation data of the fromAddress to the toAddress
    , a2b : WebData Api.Data.NeighborAddress

    -- the relation data of the toAddress to the fromAddress
    , b2a : WebData Api.Data.NeighborAddress
    , txs : Set Id
    }
