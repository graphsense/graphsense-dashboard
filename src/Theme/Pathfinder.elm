module Theme.Pathfinder exposing (Pathfinder, default)

import Css exposing (Style)


type alias Pathfinder =
    { root : List Style
    , address : List Style
    , addressHandle : List Style
    , addressLabel : List Style
    , addressRadius : Float
    , addressSpacingToLabel : Float
    }


default : Pathfinder
default =
    { root = []
    , address = []
    , addressHandle = []
    , addressLabel = []
    , addressRadius = 10
    , addressSpacingToLabel = 5
    }
