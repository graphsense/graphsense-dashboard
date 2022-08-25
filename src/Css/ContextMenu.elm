module Css.ContextMenu exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    position absolute
        :: vc.theme.contextMenu.root vc.lightmode


option : Config -> List Style
option vc =
    cursor pointer
        :: vc.theme.contextMenu.option vc.lightmode
