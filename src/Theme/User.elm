module Theme.User exposing (User, default)

import Css exposing (Style)


type alias User =
    { root : List Style
    , hovercardRoot : List Style
    , requestLimitRoot : List Style
    , requestLimit : List Style
    , requestReset : List Style
    , logoutButton : List Style
    }


default : User
default =
    { root = []
    , hovercardRoot = []
    , requestLimitRoot = []
    , requestLimit = []
    , requestReset = []
    , logoutButton = []
    }
