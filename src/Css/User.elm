module Css.User exposing (hovercardRoot, logoutButton, requestLimit, requestLimitRoot, requestReset, root)

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
