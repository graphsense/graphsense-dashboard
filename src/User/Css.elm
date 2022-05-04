module User.Css exposing (..)

import Css exposing (..)
import View.Config exposing (Config)


root : Config -> List Style
root vc =
    vc.theme.user.root


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
