module Init.Graph.History.Entry exposing (init)

import IntDict
import Model.Graph.History.Entry exposing (Model)


init : Model
init =
    { layers = IntDict.empty
    , highlights = []
    }
