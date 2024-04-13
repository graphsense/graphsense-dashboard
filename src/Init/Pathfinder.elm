module Init.Pathfinder exposing (..)

import Api.Data
import Dict
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Pathfinder.Network as Network
import Init.Search as Search
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (DetailsViewState(..), Model, Selection(..))


init : Maybe Api.Data.Stats -> Model
init stats =
    { network = Network.init
    , actors = Dict.empty
    , selection = NoSelection
    , search = Search.init (Search.initSearchAll stats)
    , dragging = NoDragging
    , transform = Transform.init
    , history = History.init
    , view = { detailsViewState = NoDetails }
    }
