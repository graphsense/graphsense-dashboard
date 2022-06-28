module Theme.Button exposing (..)

import Css exposing (Style)


type alias Button =
    { button : List Style
    , primary : List Style
    , danger : List Style
    , disabled : List Style
    }


default : Button
default =
    { button = []
    , primary = []
    , danger = []
    , disabled = []
    }
