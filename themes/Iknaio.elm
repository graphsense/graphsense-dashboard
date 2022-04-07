module Iknaio exposing (theme)

import Css exposing (..)
import Themes.Model exposing (Theme)


theme : Theme
theme =
    { header =
        batch
            [ backgroundColor <| hex "ff0000"
            ]
    }
