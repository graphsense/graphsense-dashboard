module Theme.Statusbar exposing (Statusbar, default)

import Color exposing (Color)
import Css exposing (Style)


type alias Statusbar =
    { root : List Style
    , loadingSpinner : List Style
    }


default : Statusbar
default =
    { root = []
    , loadingSpinner = []
    }
