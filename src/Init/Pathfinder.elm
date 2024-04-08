module Init.Pathfinder exposing (..)

import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Model)
import Route.Pathfinder as Route


init : Model
init =
    { networks = []
    , route = Route.Root
    , dragging = NoDragging
    , transform = Transform.init
    , history = History.init
    }
