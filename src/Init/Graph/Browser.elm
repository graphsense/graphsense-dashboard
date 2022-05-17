module Init.Graph.Browser exposing (init)

import Model.Graph.Browser exposing (..)
import Time


init : Int -> Model
init now =
    { visible = False
    , type_ = None
    , now = Time.millisToPosix now
    }
