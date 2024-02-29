module Css.Search exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


form : Config -> List Style
form vc =
    displayFlex
        :: vc.theme.search.form


frame : Config -> List Style
frame vc =
    vc.theme.search.frame


textarea : Config -> String -> List Style
textarea vc input =
    [ overflow hidden
    , resize none
    , rem 25 |> width
    ]
        ++ vc.theme.search.textarea vc.lightmode input


resultGroup : Config -> List Style
resultGroup vc =
    vc.theme.search.resultGroup


resultGroupTitle : Config -> List Style
resultGroupTitle vc =
    vc.theme.search.resultGroupTitle


resultGroupList : Config -> List Style
resultGroupList vc =
    vc.theme.search.resultGroupList


resultLine : Config -> List Style
resultLine vc =
    cursor pointer
        :: vc.theme.search.resultLine vc.lightmode


resultLineHighlighted : Config -> List Style
resultLineHighlighted vc =
    vc.theme.search.resultLineHighlighted vc.lightmode


resultLineIcon : Config -> List Style
resultLineIcon vc =
    vc.theme.search.resultLineIcon


button : Config -> List Style
button vc =
    vc.theme.search.button vc.lightmode
