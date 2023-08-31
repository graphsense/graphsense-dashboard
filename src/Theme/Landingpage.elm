module Theme.Landingpage exposing (Landingpage, default)

import Css exposing (Style)


type alias Landingpage =
    { root : List Style
    }


default : Landingpage
default =
    { root = []
    }
