module Init.Graph exposing (init)

import Config.Graph as Config
import Dict
import Init.Graph.Adding as Adding
import Init.Graph.Browser as Browser
import Init.Graph.Transform as Transform
import IntDict
import Model.Graph exposing (..)
import Plugin exposing (Plugins)


init : Plugins -> Int -> Model
init plugins now =
    { config = Config.default
    , layers = IntDict.empty
    , browser = Browser.init now
    , adding = Adding.init
    , dragging = NoDragging
    , transform = Transform.init
    , size = Nothing
    , selected = SelectedNone
    , hovered = HoveredNone
    , contextMenu = Nothing
    , plugins = Plugin.initGraph plugins
    }
