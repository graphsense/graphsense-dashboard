module Css.Dialog exposing (btnBase, optionsButtonsContainer, textWrap)

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
