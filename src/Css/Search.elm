module Css.Search exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


form : Config -> List Style
form vc =
    displayFlex
        :: vc.theme.search.form


frame : Config -> List Style
frame vc =
    [ overflow visible
    , position relative
    ]
        ++ vc.theme.search.frame


textarea : Config -> List Style
textarea vc =
    [ overflow hidden
    , resize none
    , ex 50 |> width
    ]
        ++ vc.theme.search.textarea


result : Config -> List Style
result vc =
    [ position absolute
    , zIndex <| int 200
    ]
        ++ vc.theme.search.result


loadingSpinner : Config -> List Style
loadingSpinner vc =
    [ position absolute
    ]
        ++ vc.theme.search.loadingSpinner


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
    vc.theme.search.resultLine


resultLineIcon : Config -> List Style
resultLineIcon vc =
    vc.theme.search.resultLineIcon
