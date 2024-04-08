module Css.Pathfinder exposing (..)

import Config.View as View
import Css exposing (..)


addressRoot : View.Config -> List Style
addressRoot vc =
    vc.theme.pathfinder.addressRoot
