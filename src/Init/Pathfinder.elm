module Init.Pathfinder exposing (..)

import Api.Data
import Dict
import DurationDatePicker as DatePicker
import Init.Graph.History as History
import Init.Graph.Transform as Transform
import Init.Pathfinder.Network as Network
import Init.Search as Search
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Model, Selection(..))
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Msg.Pathfinder exposing (Msg(..))
import Task
import Time


init : Maybe Api.Data.Stats -> ( Model, Cmd Msg )
init _ =
    ( { network = Network.init
      , actors = Dict.empty
      , selection = NoSelection
      , search = Search.init (Search.initSearchAddressAndTxs [ "btc", "bch", "eth", "trx", "zec", "ltc" ])
      , dragging = NoDragging
      , transform = Transform.init
      , history = History.init
      , details = Nothing
      , config = {}
      , dateRangePicker = DatePicker.init UpdateDateRangePicker
      , toDate = Nothing
      , fromDate = Nothing
      , currentTime = Time.millisToPosix 0
      , pointerTool = Drag
      , ctrlPressed = False
      }
    , Task.perform Tick Time.now
    )
