module Init.Pathfinder.Network exposing (..)

import Dict
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Network exposing (Network)


init : String -> Address -> Network
init name address =
    { name = name
    , addresses = Dict.insert address.id address Dict.empty
    , txs = Dict.empty
    }
