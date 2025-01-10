module Init.Pathfinder.Address exposing (init)

import Animation
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address, Txs(..))
import Model.Pathfinder.Id exposing (Id)
import Plugin.Update as Plugin exposing (Plugins)
import RemoteData exposing (RemoteData(..))


init : Plugins -> Id -> Coords -> Address
init plugins id { x, y } =
    { x = x
    , y = Animation.static y
    , clock = 0
    , dx = 0
    , dy = 0
    , opacity = Animation.static 1
    , id = id
    , incomingTxs = TxsNotFetched
    , outgoingTxs = TxsNotFetched
    , data = NotAsked
    , selected = False
    , exchange = Nothing
    , actor = Nothing
    , hasTags = False
    , isStartingPoint = False
    , plugins = Plugin.initAddress plugins
    }
