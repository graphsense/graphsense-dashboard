module Init.Graph exposing (init)

import Init.Graph.Adding as Adding
import Model.Graph exposing (..)
import Set


init : Model
init =
    { layers = []
    , adding = Adding.init
    }
