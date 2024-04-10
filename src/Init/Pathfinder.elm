module Init.Pathfinder exposing (..)

import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Search as Search
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Model)
import Route.Pathfinder as Route


init : Model
init =
    { network = Nothing

    --, route = Route.Root
    , selection = []
    , search = Search.init (Search.initSearchAll Nothing)
    , dragging = NoDragging
    , transform = Transform.init
    , history = History.init
    }
