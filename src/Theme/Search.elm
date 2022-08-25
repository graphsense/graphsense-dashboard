module Theme.Search exposing (Search, default)

import Css exposing (Style)


type alias Search =
    { form : List Style
    , frame : List Style
    , textarea : Bool -> String -> List Style
    , result : Bool -> List Style
    , resultGroup : List Style
    , resultGroupList : List Style
    , resultGroupTitle : List Style
    , resultLine : Bool -> List Style
    , resultLineIcon : List Style
    , loadingSpinner : List Style
    , button : List Style
    }


default : Search
default =
    { form = []
    , frame = []
    , textarea = \_ _ -> []
    , result = \_ -> []
    , resultGroup = []
    , resultGroupList = []
    , resultGroupTitle = []
    , resultLine = \_ -> []
    , resultLineIcon = []
    , loadingSpinner = []
    , button = []
    }
