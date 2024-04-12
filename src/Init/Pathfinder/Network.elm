module Init.Pathfinder.Network exposing (..)

import Dict
import Model.Pathfinder.Network exposing (Network)


init : Network
init =
    { addresses = Dict.empty
    , txs = Dict.empty
    }
