module Css.Header exposing (..)

import Config.View exposing (Config)
import Css exposing (..)
import Util.Css


header : Config -> List Style
header vc =
    [ displayFlex
    , flexDirection row
    , justifyContent spaceBetween
    , zIndex <| int <| Util.Css.zIndexMainValue + 1
    ]
        ++ vc.theme.header vc.lightmode


headerLogoWrap : Config -> List Style
headerLogoWrap vc =
    vc.theme.headerLogoWrap


headerLogo : Config -> List Style
headerLogo vc =
    vc.theme.headerLogo


headerTitle : Config -> List Style
headerTitle vc =
    vc.theme.headerTitle
