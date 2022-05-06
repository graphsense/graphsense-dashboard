module Graph.Init.Adding exposing (init)

import Graph.Model.Adding exposing (..)
import Set


init : Model
init =
    { addresses = Set.empty
    , entities = Set.empty
    , labels = Set.empty
    }
