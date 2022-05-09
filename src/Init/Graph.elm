module Init.Graph exposing (init)

import Dict
import Init.Graph.Adding as Adding
import Model.Graph exposing (..)


init : Model
init =
    { layers = []
    , adding = Adding.init
    , colors = Dict.empty
    }
