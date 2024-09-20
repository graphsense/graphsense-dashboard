module Init.Graph.History exposing (init)

import Model.Graph.History exposing (Model)


init : Model entry
init =
    { past = []
    , future = []
    }
