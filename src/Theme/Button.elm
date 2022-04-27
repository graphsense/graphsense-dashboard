module Theme.Button exposing (..)

import Css exposing (Style)


type alias Button =
    { base : List Style
    , primary : List Style
    , danger : List Style
    , disabled : List Style
    }


default : Button
default =
    { base = []
    , primary = []
    , danger = []
    , disabled = []
    }
