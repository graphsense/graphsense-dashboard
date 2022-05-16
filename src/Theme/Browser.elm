module Theme.Browser exposing (Browser, default)

import Color exposing (Color)
import Css exposing (Style)


type alias Browser =
    { root : List Style
    , frame : Bool -> List Style
    , propertyBoxTable : List Style
    , propertyBoxRow : List Style
    , propertyBoxKey : List Style
    , propertyBoxValue : List Style
    , propertyBoxEntityId : List Style
    }


default : Browser
default =
    { root = []
    , frame = \_ -> []
    , propertyBoxTable = []
    , propertyBoxRow = []
    , propertyBoxKey = []
    , propertyBoxValue = []
    , propertyBoxEntityId = []
    }
