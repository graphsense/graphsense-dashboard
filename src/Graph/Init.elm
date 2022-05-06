module Graph.Init exposing (init)

import Graph.Init.Adding as Adding
import Graph.Model exposing (..)
import Set


init : Model
init =
    { layers = []
    , adding = Adding.init
    }
