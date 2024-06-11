module Init.Pathfinder.Network exposing (..)

import Dict
import Model.Pathfinder.Network exposing (Network)
import Set


init : Network
init =
    { addresses = Dict.empty
    , txs = Dict.empty
    , animatedAddresses = Set.empty
    , animatedTxs = Set.empty
    }
