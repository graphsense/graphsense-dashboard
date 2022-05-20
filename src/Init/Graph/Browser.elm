module Init.Graph.Browser exposing (..)

import Model.Graph.Browser exposing (..)
import Model.Graph.Table exposing (Table)
import Table
import Time


init : Int -> Model
init now =
    { visible = False
    , type_ = None
    , now = Time.millisToPosix now
    }
