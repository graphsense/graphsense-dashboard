module Locale.Init exposing (init)

import Dict
import Locale.Model as Model exposing (Model)


init : Model
init =
    { mapping = Dict.empty
    }
