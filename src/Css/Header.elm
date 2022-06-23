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


headerLogoWrap : Config -> List Style
headerLogoWrap vc =
    vc.theme.headerLogoWrap


headerLogo : Config -> List Style
headerLogo vc =
    vc.theme.headerLogo


headerTitle : Config -> List Style
headerTitle vc =
    vc.theme.headerTitle
