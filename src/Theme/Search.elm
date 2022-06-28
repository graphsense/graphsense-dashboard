module Theme.Search exposing (Search, default)

import Css exposing (Style)


type alias Search =
    { form : List Style
    , frame : List Style
    , textarea : String -> List Style
    , result : List Style
    , resultGroup : List Style
    , resultGroupList : List Style
    , resultGroupTitle : List Style
    , resultLine : List Style
    , resultLineIcon : List Style
    , loadingSpinner : List Style
    , button : List Style
    }


default : Search
default =
    { form = []
    , frame = []
    , textarea = \_ -> []
    , result = []
    , resultGroup = []
    , resultGroupList = []
    , resultGroupTitle = []
    , resultLine = []
    , resultLineIcon = []
    , loadingSpinner = []
    , button = []
    }
