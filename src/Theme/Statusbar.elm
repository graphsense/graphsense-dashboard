module Theme.Statusbar exposing (Statusbar, default)

import Color exposing (Color)
import Css exposing (Style)


type alias Statusbar =
    { root : Bool -> List Style
    , loadingSpinner : List Style
    , log : Bool -> List Style
    , close : List Style
    }


default : Statusbar
default =
    { root = \_ -> []
    , loadingSpinner = []
    , log = \_ -> []
    , close = []
    }
