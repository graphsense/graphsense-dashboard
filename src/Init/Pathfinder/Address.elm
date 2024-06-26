module Init.Pathfinder.Address exposing (..)

import Animation
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..))
import Set


init : Id -> Coords -> Address
init id { x, y } =
    { x = x
    , y = Animation.static y
    , clock = 0
    , dx = 0
    , dy = 0
    , opacity = Animation.static 1
    , id = id
    , incomingTxs = Set.empty
    , outgoingTxs = Set.empty
    , data = NotAsked
    , selected = False
    , exchange = Nothing
    }
