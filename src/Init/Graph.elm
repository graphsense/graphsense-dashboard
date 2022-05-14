module Init.Graph exposing (init)

import Config.Graph as Config
import Dict
import Init.Graph.Adding as Adding
import Init.Graph.Transform as Transform
import IntDict
import Model.Graph exposing (..)


init : Model
init =
    { config = Config.default
    , layers = IntDict.empty
    , adding = Adding.init
    , dragging = NoDragging
    , mouse = { x = 0, y = 0 }
    , transform = Transform.init
    , width = 1200
    , height = 800
    }
