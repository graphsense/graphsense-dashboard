module Init.Graph.Adding exposing (init)

import Model.Graph.Adding exposing (..)
import Set


init : Model
init =
    { addresses = Set.empty
    , entities = Set.empty
    , labels = Set.empty
    }
