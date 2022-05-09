module Init.Graph.Transform exposing (..)

import Model.Graph.Transform exposing (..)


init : Model
init =
    { transform =
        { x = 0
        , y = 0
        , z = 1
        }
    , dragging = NoDragging
    }
