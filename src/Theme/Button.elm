module Theme.Button exposing (..)

import Css exposing (Style)


type alias Button =
    { button : Bool -> List Style
    , primary : Bool -> List Style
    , danger : Bool -> List Style
    , disabled : Bool -> List Style
    }


default : Button
default =
    { button = \_ -> []
    , primary = \_ -> []
    , danger = \_ -> []
    , disabled = \_ -> []
    }
