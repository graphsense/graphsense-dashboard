module Theme.Dialog exposing (Dialog, default)

import Css exposing (Style)


type alias Dialog =
    { dialog : Bool -> List Style
    , buttons : List Style
    , heading : List Style
    , part : List Style
    , headRow : Bool -> List Style
    , body : List Style
    , headRowText : List Style
    , headRowClose : Bool -> List Style
    }


default : Dialog
default =
    { dialog = \_ -> []
    , buttons = []
    , heading = []
    , part = []
    , headRow = \_ -> []
    , body = []
    , headRowText = []
    , headRowClose = \_ -> []
    }
