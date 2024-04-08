module Theme.Pathfinder exposing (Pathfinder, default)

import Css exposing (Style)


type alias Pathfinder =
    { root : List Style
    , addressRoot : List Style
    }


default : Pathfinder
default =
    { root = []
    , addressRoot = []
    }
