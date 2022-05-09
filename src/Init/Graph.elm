module Init.Graph exposing (init)

import Dict
import Init.Graph.Adding as Adding
import Init.Graph.Transform as Transform
import Model.Graph exposing (..)


init : Model
init =
    { layers = []
    , adding = Adding.init
    , colors = Dict.empty
    , transform = Transform.init
    , width = 1200
    , height = 800
    }
