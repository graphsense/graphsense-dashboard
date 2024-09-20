module Init.Statusbar exposing (init)

import Dict
import Model.Statusbar exposing (..)


init : Model
init =
    { messages = Dict.empty
    , log = []
    , visible = False
    , lastBlocks = []
    }
