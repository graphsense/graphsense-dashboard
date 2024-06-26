module Css.Dialog exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


dialog : Config -> List Style
dialog vc =
    vc.theme.dialog.dialog vc.lightmode


buttons : Config -> List Style
buttons vc =
    vc.theme.dialog.buttons


button : Config -> List Style
button vc =
    [ px 100 |> minWidth, pct 50 |> width ]


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
