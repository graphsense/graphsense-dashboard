module Theme.Statusbar exposing (Statusbar, default)

import Css exposing (Style)


type alias Statusbar =
    { root : Bool -> Bool -> List Style
    , loadingSpinner : List Style
    , log : Bool -> Bool -> List Style
    , logIcon : Bool -> Bool -> List Style
    , close : Bool -> List Style
    }


default : Statusbar
default =
    { root = \_ _ -> []
    , loadingSpinner = []
    , log = \_ _ -> []
    , logIcon = \_ _ -> []
    , close = \_ -> []
    }
