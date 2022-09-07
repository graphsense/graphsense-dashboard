module Css.User exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    vc.theme.user.root vc.lightmode


hovercardRoot : Config -> List Style
hovercardRoot vc =
    vc.theme.user.hovercardRoot


requestLimitRoot : Config -> List Style
requestLimitRoot vc =
    vc.theme.user.requestLimitRoot


requestLimit : Config -> List Style
requestLimit vc =
    vc.theme.user.requestLimit


requestReset : Config -> List Style
requestReset vc =
    vc.theme.user.requestReset


logoutButton : Config -> List Style
logoutButton vc =
    vc.theme.user.logoutButton vc.lightmode


lightmodeLabel : Config -> List Style
lightmodeLabel vc =
    vc.theme.user.lightmodeLabel


lightmodeRoot : Config -> List Style
lightmodeRoot vc =
    vc.theme.user.lightmodeRoot
