module Css.Header exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


header : Config -> List Style
header vc =
    [ displayFlex
    , flexDirection row
    , justifyContent spaceBetween
    ]
        ++ vc.theme.header


headerLogo : Config -> List Style
headerLogo vc =
    vc.theme.headerLogo
