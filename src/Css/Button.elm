module Css.Button exposing (iconButton)

import Config.View exposing (Config)
import Css exposing (..)


iconButton : Config -> List Style
iconButton vc =
    cursor pointer :: vc.theme.button.iconButton vc.lightmode
