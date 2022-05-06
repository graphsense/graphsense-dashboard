module Theme.Graph exposing (Graph, default)

import Css exposing (Style)


type alias Graph =
    { root : List Style
    , addressFlags : List Style
    , addressLabel : List Style
    , addressRect : List Style
    , addressRoot : List Style
    , graphRoot : List Style
    , navbar : List Style
    , navbarLeft : List Style
    , navbarRight : List Style
    , tool : List Style
    }


default : Graph
default =
    { root = []
    , addressFlags = []
    , addressLabel = []
    , addressRect = []
    , addressRoot = []
    , graphRoot = []
    , navbar = []
    , navbarLeft = []
    , navbarRight = []
    , tool = []
    }
