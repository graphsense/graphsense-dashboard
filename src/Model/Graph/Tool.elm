module Model.Graph.Tool exposing (Status(..), Toolbox(..))

import Config.Graph exposing (Config)
import Model.Graph.Legend as Legend


type Toolbox
    = Legend (List Legend.Item)
    | Configuration Config
    | Export
    | Import
    | Highlighter


type Status
    = Active
    | Inactive
    | Disabled
