module Css.Dialog exposing (body, btnBase, button, buttons, dialog, headRow, headRowClose, headRowText, heading, optionsButtonsContainer, part, singleButton, textWrap)

import Config.View exposing (Config)
import Css exposing (..)


dialog : Config -> List Style
dialog vc =
    vc.theme.dialog.dialog vc.lightmode


buttons : Config -> List Style
buttons vc =
    vc.theme.dialog.buttons


button : Config -> List Style
button _ =
    [ px 100 |> minWidth, pct 50 |> width ]


btnBase : Config -> List Style
btnBase _ =
    [ Css.cursor Css.pointer ]


textWrap : Config -> List Style
textWrap _ =
    [ Css.whiteSpace Css.normal ]


optionsButtonsContainer : List Style
optionsButtonsContainer =
    [ Css.displayFlex, Css.flexDirection Css.row, Css.justifyContent Css.spaceAround, Css.width (Css.pct 100) ]


singleButton : Config -> List Style
singleButton vc =
    vc.theme.dialog.singleButton


part : Config -> List Style
part vc =
    vc.theme.dialog.part


heading : Config -> List Style
heading vc =
    vc.theme.dialog.heading


headRow : Config -> List Style
headRow vc =
    vc.theme.dialog.headRow vc.lightmode


body : Config -> List Style
body vc =
    vc.theme.dialog.body


headRowText : Config -> List Style
headRowText vc =
    vc.theme.dialog.headRowText


headRowClose : Config -> List Style
headRowClose vc =
    vc.theme.dialog.headRowClose vc.lightmode
