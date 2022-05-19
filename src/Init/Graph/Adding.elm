module Init.Graph.Adding exposing (init)

import Dict
import Model.Graph.Adding exposing (..)
import Set


init : Model
init =
    { addresses = Dict.empty
    , entities = Set.empty
    , labels = Set.empty
    }
