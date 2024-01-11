module Init.Graph.History exposing (..)

import IntDict
import Model.Graph.History exposing (Model)


init : Model
init =
    { past = []
    , future = []
    }
