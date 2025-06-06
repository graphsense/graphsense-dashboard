module Model.Pathfinder.AggEdge exposing (AggEdge)

import Animation exposing (Animation, Clock)
import Api.Data
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)


type alias AggEdge =
    { id : ( Id, Id )
    , fromAddress : Maybe Address
    , toAddress : Maybe Address
    , data : Api.Data.NeighborAddress
    }
