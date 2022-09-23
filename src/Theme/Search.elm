module Theme.Search exposing (Search, default)

import Css exposing (Style)


type alias Search =
    { form : List Style
    , frame : List Style
    , textarea : Bool -> String -> List Style
    , resultGroup : List Style
    , resultGroupList : List Style
    , resultGroupTitle : List Style
    , resultLine : Bool -> List Style
    , resultLineIcon : List Style
    , button : List Style
    }


default : Search
default =
    { form = []
    , frame = []
    , textarea = \_ _ -> []
    , resultGroup = []
    , resultGroupList = []
    , resultGroupTitle = []
    , resultLine = \_ -> []
    , resultLineIcon = []
    , button = []
    }
