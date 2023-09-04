module Theme.Landingpage exposing (Landingpage, default)

import Color
import Css exposing (Style)
import Theme.SwitchableColor exposing (SwitchableColor)


type alias Landingpage =
    { root : List Style
    , frame : Bool -> List Style
    , searchRoot : List Style
    , searchTextarea : Bool -> List Style
    , rule : Bool -> List Style
    , ruleColor : SwitchableColor
    , loadBox : Bool -> List Style
    , loadBoxIcon : Bool -> List Style
    , loadBoxText : Bool -> List Style
    }


default : Landingpage
default =
    { root = []
    , frame = \_ -> []
    , searchRoot = []
    , searchTextarea = \_ -> []
    , rule = \_ -> []
    , ruleColor =
        { dark = Color.rgb 0 0 0
        , light = Color.rgb 1 1 1
        }
    , loadBox = \_ -> []
    , loadBoxIcon = \_ -> []
    , loadBoxText = \_ -> []
    }
