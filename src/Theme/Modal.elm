module Theme.Modal exposing (Modal, default)

import Css exposing (Style)


type alias Modal =
    { heading : List Style
    , part : List Style
    }


default : Modal
default =
    { heading = []
    , part = []
    }
