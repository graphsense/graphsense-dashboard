module Init.Pathfinder.AggEdge exposing (init, initId)

-- exposing (TracingMode(..))

import Config.Pathfinder as Pathfinder
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..))
import Set
import Tuple exposing (first, second)


init : Pathfinder.Config -> Id -> Id -> AggEdge
init _ a b =
    let
        id =
            initId a b
    in
    { a = first id
    , b = second id
    , aAddress = Nothing
    , bAddress = Nothing
    , a2b = NotAsked
    , b2a = NotAsked
    , txs = Set.empty
    , selected = False
    , hovered = False
    , alwaysShow = False -- pc.tracingMode == AggregateTracingMode
    }


initId : Id -> Id -> ( Id, Id )
initId a b =
    if a < b then
        ( a, b )

    else
        ( b, a )
