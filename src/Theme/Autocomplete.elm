module Theme.Autocomplete exposing (Autocomplete, default)

import Css exposing (Style)


type alias Autocomplete =
    { frame : List Style
    , result : Bool -> List Style
    , loadingSpinner : List Style
    }


default : Autocomplete
default =
    { frame = []
    , result = \_ -> []
    , loadingSpinner = []
    }
