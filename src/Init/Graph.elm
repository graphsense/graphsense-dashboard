module Init.Graph exposing (init)

import Config.Graph as Config
import Dict
import Init.Graph.Adding as Adding
import Init.Graph.Browser as Browser
import Init.Graph.Transform as Transform
import IntDict
import Model.Graph exposing (..)


init : Int -> Model
init now =
    { config = Config.default
    , layers = IntDict.empty
    , browser = Browser.init now
    , adding = Adding.init
    , dragging = NoDragging
    , transform = Transform.init
    , size = Nothing
    , selected = Nothing
    }
