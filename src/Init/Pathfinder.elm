module Init.Pathfinder exposing (..)

import Api.Data
import Config.UserSettings exposing (UserSettings)
import Dict
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Pathfinder.Network as Network
import Init.Search as Search
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Hovered(..), Model, Selection(..))
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Msg.Pathfinder exposing (Msg(..))
import Set exposing (..)
import Task
import Time


init : UserSettings -> Maybe Api.Data.Stats -> ( Model, Cmd Msg )
init us _ =
    ( { network = Network.init
      , actors = Dict.empty
      , tagSummaries = Dict.empty
      , colors = Colors.init
      , clusters = Dict.empty
      , selection = NoSelection
      , hovered = NoHover
      , search = Search.init (Search.initSearchAddressAndTxs [ "btc", "bch", "zec", "ltc", "eth" ])
      , dragging = NoDragging
      , transform = Transform.init
      , history = History.init
      , details = Nothing
      , config =
            { displaySettingsHovercard = Nothing
            , isClusterDetailsOpen = False
            , displayAllTagsInDetails = False
            }
      , currentTime = Time.millisToPosix 0
      , pointerTool = Drag
      , ctrlPressed = False
      , isDirty = False
      , tooltip = Nothing
      }
    , Task.perform Tick Time.now
    )
