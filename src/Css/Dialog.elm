module Css.Dialog exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


part : Config -> List Style
part vc =
    vc.theme.dialog.part


heading : Config -> List Style
heading vc =
    vc.theme.dialog.heading
