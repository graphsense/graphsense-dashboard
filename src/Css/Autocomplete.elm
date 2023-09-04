module Css.Autocomplete exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


frame : Config -> List Style
frame vc =
    [ overflow visible
    , position relative
    ]
        ++ vc.theme.autocomplete.frame


result : Config -> List Style
result vc =
    [ position absolute
    , zIndex <| int 200
    ]
        ++ vc.theme.autocomplete.result vc.lightmode


loadingSpinner : Config -> List Style
loadingSpinner vc =
    (position absolute)
        :: vc.theme.autocomplete.loadingSpinner
