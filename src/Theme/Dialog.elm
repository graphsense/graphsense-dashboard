module Theme.Dialog exposing (Dialog, default)

import Css exposing (Style)


type alias Dialog =
    { heading : List Style
    , part : List Style
    }


default : Dialog
default =
    { heading = []
    , part = []
    }
