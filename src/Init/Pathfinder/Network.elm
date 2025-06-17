module Init.Pathfinder.Network exposing (init)

import Dict
import Model.Pathfinder.Network exposing (Network)
import Set


init : Network
init =
    { addresses = Dict.empty
    , txs = Dict.empty
    , aggEdges = Dict.empty
    , addressAggEdgeMap = Dict.empty
    , animatedAddresses = Set.empty
    , animatedTxs = Set.empty
    }
