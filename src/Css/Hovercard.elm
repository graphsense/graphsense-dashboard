module Css.Hovercard exposing (..)

import Config.View exposing (Config)
import Css exposing (..)
import Util.View exposing (toCssColor)


root : Config -> List Style
root vc =
    [ backgroundColor <| toCssColor (vc.theme.hovercard vc.lightmode).backgroundColor
    , borderColor <| toCssColor (vc.theme.hovercard vc.lightmode).borderColor
    ]
