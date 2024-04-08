module Init.Graph.History exposing (..)

import Model.Graph.History exposing (Model)


init : Model entry
init =
    { past = []
    , future = []
    }
