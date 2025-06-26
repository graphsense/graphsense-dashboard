module Model.Pathfinder.AggEdge exposing (AggEdge, idToString)

import Api.Data
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
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
    , hovered : Bool
    , alwaysShow : Bool
    }


idToString : ( Id, Id ) -> String
idToString ( a, b ) =
    Id.toString a ++ "_" ++ Id.toString b
