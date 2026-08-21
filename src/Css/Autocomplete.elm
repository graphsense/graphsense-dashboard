module Css.Autocomplete exposing (result)

import Config.View exposing (Config)
import Css exposing (..)


result : Config -> List Style
result vc =
    [ position absolute
    , zIndex <| int 200
    ]
        ++ vc.theme.autocomplete.result vc.lightmode
