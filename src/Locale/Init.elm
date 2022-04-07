module Locale.Init exposing (init)

import Locale.Model as Model exposing (Model)


init : Model
init =
    { getString = \str -> str
    }
