module Theme.Button exposing (Button, default)

import Css exposing (Style)


type alias Button =
    { button : Bool -> List Style
    , neutral : Bool -> List Style
    , primary : Bool -> List Style
    , danger : Bool -> List Style
    , disabled : Bool -> List Style
    , iconButton : Bool -> List Style
    }


default : Button
default =
    { button = \_ -> []
    , neutral = \_ -> []
    , primary = \_ -> []
    , danger = \_ -> []
    , disabled = \_ -> []
    , iconButton = \_ -> []
    }
