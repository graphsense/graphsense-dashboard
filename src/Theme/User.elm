module Theme.User exposing (User, default)

import Css exposing (Style)


type alias User =
    { root : Bool -> List Style
    , hovercardRoot : List Style
    , requestLimitRoot : List Style
    , requestLimit : List Style
    , requestReset : List Style
    , logoutButton : Bool -> List Style
    }


default : User
default =
    { root = \_ -> []
    , hovercardRoot = []
    , requestLimitRoot = []
    , requestLimit = []
    , requestReset = []
    , logoutButton = \_ -> []
    }
