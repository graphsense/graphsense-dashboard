module Theme.Dialog exposing (Dialog, default)

import Css exposing (Style)


type alias Dialog =
    { dialog : List Style
    , buttons : List Style
    , heading : List Style
    , part : List Style
    , headRow : List Style
    , body : List Style
    , headRowText : List Style
    , headRowClose : List Style
    }


default : Dialog
default =
    { dialog = []
    , buttons = []
    , heading = []
    , part = []
    , headRow = []
    , body = []
    , headRowText = []
    , headRowClose = []
    }
