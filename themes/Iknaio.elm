module Iknaio exposing (theme)

import Css exposing (..)
import Themes.Model exposing (Theme)


theme : Theme
theme =
    { scale = 10
    , header =
        batch
            [ backgroundColor <| hex "ff0000"
            ]
    , stats =
        { root = batch []
        }
    }
