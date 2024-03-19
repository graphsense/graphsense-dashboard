module Css.Button exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


button : Config -> List Style
button vc =
    cursor pointer :: vc.theme.button.button vc.lightmode


primary : Config -> List Style
primary vc =
    button vc ++ vc.theme.button.primary vc.lightmode


neutral : Config -> List Style
neutral vc =
    button vc ++ vc.theme.button.neutral vc.lightmode


danger : Config -> List Style
danger vc =
    button vc ++ vc.theme.button.danger vc.lightmode


disabled : Config -> List Style
disabled vc =
    button vc ++ [ cursor notAllowed ] ++ vc.theme.button.disabled vc.lightmode
