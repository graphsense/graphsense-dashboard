module Init.Statusbar exposing (..)

import Dict
import Model.Statusbar exposing (..)


init : Model
init =
    { messages = Dict.empty
    }
