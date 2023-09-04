module Css.Landingpage exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    vc.theme.landingpage.root


frame : Config -> List Style
frame vc =
    vc.theme.landingpage.frame vc.lightmode


searchRoot : Config -> List Style
searchRoot vc =
    vc.theme.landingpage.searchRoot


searchTextarea : Config -> List Style
searchTextarea vc =
    vc.theme.landingpage.searchTextarea vc.lightmode


rule : Config -> List Style
rule vc =
    vc.theme.landingpage.rule vc.lightmode


loadBox : Config -> List Style
loadBox vc =
    vc.theme.landingpage.loadBox vc.lightmode


loadBoxIcon : Config -> List Style
loadBoxIcon vc =
    vc.theme.landingpage.loadBoxIcon vc.lightmode


loadBoxText : Config -> List Style
loadBoxText vc =
    vc.theme.landingpage.loadBoxText vc.lightmode
