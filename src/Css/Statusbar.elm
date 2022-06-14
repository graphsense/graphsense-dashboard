module Css.Statusbar exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    vc.theme.statusbar.root


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.statusbar.loadingSpinner
