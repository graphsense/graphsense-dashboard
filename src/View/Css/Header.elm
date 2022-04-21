module View.Css.Header exposing (..)

import Css exposing (..)
import View.Config exposing (Config)


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
