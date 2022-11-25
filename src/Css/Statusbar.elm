module Css.Statusbar exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> Bool -> List Style
root vc visible =
    vc.theme.statusbar.root vc.lightmode visible


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.statusbar.loadingSpinner


log : Config -> Bool -> List Style
log vc noerror =
    vc.theme.statusbar.log vc.lightmode noerror


logIcon : Config -> Bool -> List Style
logIcon vc noerror =
    vc.theme.statusbar.logIcon vc.lightmode noerror


close : Config -> List Style
close vc =
    vc.theme.statusbar.close vc.lightmode
