module Init.Graph exposing (init)

import Config.Graph as Config
import Config.UserSettings exposing (UserSettings)
import Dict
import Init.Graph.Adding as Adding
import Init.Graph.Browser as Browser
import Init.Graph.Highlighter as Highlighter
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import IntDict
import Model.Graph exposing (..)
import Model.Graph.Tool exposing (Toolbox(..))
import Route.Graph


init : UserSettings -> Int -> Model
init us now =
    { config = Config.init us.addressLabel us.edgeLabel us.showClusterShadowLinks us.showAddressShadowLinks us.showDatesInUserLocale us.showZeroValueTxs
    , layers = IntDict.empty
    , route = Route.Graph.rootRoute
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
    , history = History.init
    , highlights = Highlighter.init
    , selectIfLoaded = Nothing
    }
