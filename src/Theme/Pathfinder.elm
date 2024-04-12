module Theme.Pathfinder exposing (Pathfinder, default)

import Css exposing (Style)


type alias Pathfinder =
    { root : List Style
    , address : List Style
    , addressHandle : List Style
    , addressRadius : Float
    }


default : Pathfinder
default =
    { root = []
    , address = []
    , addressHandle = []
    , addressRadius = 10
    }
