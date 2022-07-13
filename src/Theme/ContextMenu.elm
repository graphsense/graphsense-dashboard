module Theme.ContextMenu exposing (ContextMenu, default)

import Color exposing (Color)
import Css exposing (Style)


type alias ContextMenu =
    { root : List Style
    , option : List Style
    }


default : ContextMenu
default =
    { root = []
    , option = []
    }
