module Init.Graph exposing (init)

import Config.Graph as Config
import Dict
import Init.Graph.Adding as Adding
import Init.Graph.Browser as Browser
import Init.Graph.Highlighter as Highlighter
import Init.Graph.Transform as Transform
import IntDict
import Model.Graph exposing (..)
import Model.Graph.Tool exposing (Toolbox(..))


init : Int -> Model
init now =
    { config = Config.default
    , layers = IntDict.empty
    , browser = Browser.init now
    , adding = Adding.init
    , dragging = NoDragging
    , transform = Transform.init
    , selected = SelectedNone
    , hovered = HoveredNone
    , contextMenu = Nothing
    , tag = Nothing
    , userAddressTags = Dict.empty
    , activeTool =
        { element = Nothing
        , toolbox = Legend []
        }
    , search = Nothing
    , history = History [] []
    , highlights = Highlighter.init
    , selectIfLoaded = Nothing
    }
