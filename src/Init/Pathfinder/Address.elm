module Init.Pathfinder.Address exposing (..)

import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..))


init : Id -> Coords -> Address
init id { x, y } =
    { x = x
    , y = y
    , id = id
    , transactions = NotAsked
    }
