module Init.Graph.Browser exposing (init)

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
