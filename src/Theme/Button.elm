module Theme.Button exposing (..)

import Css exposing (Style)


type alias Button =
    { button : Bool -> List Style
    , neutral : Bool -> List Style
    , primary : Bool -> List Style
    , danger : Bool -> List Style
    , disabled : Bool -> List Style
    }


default : Button
default =
    { button = \_ -> []
    , neutral = \_ -> []
    , primary = \_ -> []
    , danger = \_ -> []
    , disabled = \_ -> []
    }
