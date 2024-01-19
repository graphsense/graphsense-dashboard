module Init.Graph.History.Entry exposing (..)

import IntDict
import Model.Graph.History.Entry exposing (Model)


init : Model
init =
    { layers = IntDict.empty
    , highlights = []
    }
