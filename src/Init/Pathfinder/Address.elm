module Init.Pathfinder.Address exposing (..)

import Api.Data
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..))


init : Id -> Api.Data.Address -> Coords -> Address
init id data { x, y } =
    { x = x
    , y = y
    , id = id
    , transactions = NotAsked
    , data = data
    }
