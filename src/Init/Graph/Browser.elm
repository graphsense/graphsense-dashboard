module Init.Graph.Browser exposing (..)

import IntDict
import Model.Graph.Browser exposing (..)
import Time


init : Int -> Model
init now =
    { visible = False
    , type_ = None
    , now = Time.millisToPosix now
    , height = Nothing
    , layers = IntDict.empty
    , width = 0
    }
