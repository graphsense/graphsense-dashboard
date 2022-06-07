module Theme.Dialog exposing (Dialog, default)

import Css exposing (Style)


type alias Dialog =
    { heading : List Style
    , part : List Style
    , headRow : List Style
    , body : List Style
    , headRowText : List Style
    , headRowClose : List Style
    }


default : Dialog
default =
    { heading = []
    , part = []
    , headRow = []
    , body = []
    , headRowText = []
    , headRowClose = []
    }
