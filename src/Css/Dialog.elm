module Css.Dialog exposing (btnBase, heading, optionsButtonsContainer, part, textWrap)

import Config.View exposing (Config)
import Css exposing (..)


btnBase : Config -> List Style
btnBase _ =
    [ Css.cursor Css.pointer ]


textWrap : Config -> List Style
textWrap _ =
    [ Css.whiteSpace Css.normal ]


optionsButtonsContainer : List Style
optionsButtonsContainer =
    [ Css.displayFlex, Css.flexDirection Css.row, Css.justifyContent Css.spaceAround, Css.width (Css.pct 100) ]


part : Config -> List Style
part vc =
    vc.theme.dialog.part


heading : Config -> List Style
heading vc =
    vc.theme.dialog.heading
