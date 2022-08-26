module Theme.ContextMenu exposing (ContextMenu, default)

import Color exposing (Color)
import Css exposing (Style)


type alias ContextMenu =
    { root : Bool -> List Style
    , option : Bool -> List Style
    }


default : ContextMenu
default =
    { root = \_ -> []
    , option = \_ -> []
    }
