module Init.Pathfinder.Address exposing (init)

import Animation
import Dict
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Address exposing (Address, AddressServiceType(..), Txs(..))
import Model.Pathfinder.Id as Id exposing (Id)
import Plugin.Update as Plugin
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
    , incomingTxs = TxsNotFetched
    , outgoingTxs = TxsNotFetched
    , data = NotAsked
    , selected = False
    , clusterSiblingHovered = False
    , exchange = Nothing
    , actor = Nothing
    , hasTags = False
    , hasClusterTagsOnly = False
    , networks = Dict.singleton (Id.network id) (Set.singleton (Id.id id))
    , isStartingPoint = False
    , plugins = Plugin.initAddress
    , clusterColor = Nothing
    , addressServiceType = UnknownService
    }
