module Init.Pathfinder.AggEdge exposing (init, initId)

import Config.Pathfinder as Pathfinder exposing (TracingMode(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..))
import Set
import Tuple exposing (first, second)


init : Pathfinder.Config -> Id -> Id -> Maybe Address -> Maybe Address -> AggEdge
init pc a b aAddress bAddress =
    let
        id =
            initId a b
    in
    { a = first id
    , b = second id
    , aAddress = aAddress
    , bAddress = bAddress
    , a2b = NotAsked
    , b2a = NotAsked
    , txs = Set.empty
    , selected = False
    , alwaysShow = pc.tracingMode == AggregateTracingMode
    }


initId : Id -> Id -> ( Id, Id )
initId a b =
    if a < b then
        ( a, b )

    else
        ( b, a )
