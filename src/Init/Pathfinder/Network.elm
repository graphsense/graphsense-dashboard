module Init.Pathfinder.Network exposing (init)

import Dict
import Init.Pathfinder.Relation as Relation
import Model.Pathfinder.Network exposing (Network)
import Set


init : Network
init =
    { addresses = Dict.empty
    , txs = Dict.empty
    , relations = Relation.init
    , animatedAddresses = Set.empty
    , animatedTxs = Set.empty
    }
