module Themes.Model exposing (Theme)

import Css exposing (Style)


type alias Theme =
    { scale : Float
    , header : Style
    , stats :
        { root : Style
        }
    }
