module Init.Graph.Highlighter exposing (init)

import Model.Graph.Highlighter exposing (..)


init : Model
init =
    { highlights = []
    , selected = Nothing
    }
