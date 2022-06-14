module Css.Statusbar exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> Bool -> List Style
root vc visible =
    vc.theme.statusbar.root visible


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.statusbar.loadingSpinner


log : Config -> Bool -> List Style
log vc noerror =
    vc.theme.statusbar.log noerror


close : Config -> List Style
close vc =
    vc.theme.statusbar.close
