module Init.Pathfinder exposing (init)

import Config.Pathfinder exposing (TracingMode(..))
import Dict
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Pathfinder.Network as Network
import Init.Search as Search
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Hovered(..), Model, Selection(..))
import Model.Pathfinder.CheckingNeighbors as CheckingNeighbors
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Msg.Pathfinder exposing (Msg)
import Util.Annotations as Annotations


init : { x | snapToGrid : Maybe Bool, highlightClusterFriends : Maybe Bool, tracingMode : Maybe TracingMode } -> ( Model, Cmd Msg )
init us =
    ( { network = Network.init
      , actors = Dict.empty
      , tagSummaries = Dict.empty
      , colors = Colors.init
      , annotations = Annotations.empty
      , clusters = Dict.empty
      , selection = NoSelection
      , hovered = NoHover
      , search = Search.init (Search.initSearchAddressAndTxs Nothing)
      , dragging = NoDragging
      , transform = Transform.init
      , history = History.init
      , details = Nothing
      , config =
            { snapToGrid = us.snapToGrid |> Maybe.withDefault False
            , highlightClusterFriends = us.highlightClusterFriends |> Maybe.withDefault True
            , tracingMode = us.tracingMode |> Maybe.withDefault TransactionTracingMode
            }
      , pointerTool = Drag
      , modPressed = False
      , isDirty = False
      , helpDropdownOpen = False
      , toolbarHovercard = Nothing
      , contextMenu = Nothing
      , name = "graph"
      , checkingNeighbors = CheckingNeighbors.init
      }
    , Cmd.none
    )
