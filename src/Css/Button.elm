module Css.Button exposing (..)

import Config.View exposing (Config)
import Css exposing (..)
import Util.Css


button : Config -> List Style
button vc =
    cursor pointer :: vc.theme.button.button vc.lightmode


primary : Config -> List Style
primary vc =
    button vc ++ vc.theme.button.primary vc.lightmode


danger : Config -> List Style
danger vc =
    button vc ++ vc.theme.button.danger vc.lightmode


disabled : Config -> List Style
disabled vc =
    cursor notAllowed :: vc.theme.button.disabled vc.lightmode
