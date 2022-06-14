module Css.View exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


heading2 : Config -> List Style
heading2 vc =
    vc.theme.heading2


body : Config -> List Style
body vc =
    [ Css.height <| vh 100
    , displayFlex
    , flexDirection column
    , overflow Css.hidden
    ]
        ++ vc.theme.body


sectionBelowHeader : Config -> List Style
sectionBelowHeader vc =
    [ displayFlex
    , flexDirection row
    , flexGrow (num 1)
    ]
        ++ vc.theme.sectionBelowHeader


sidebar : Config -> List Style
sidebar vc =
    displayFlex
        :: flexDirection column
        :: vc.theme.sidebar


sidebarIcon : Config -> Bool -> List Style
sidebarIcon vc active =
    vc.theme.sidebarIcon active


main_ : Config -> List Style
main_ vc =
    [ flexGrow (num 1)
    ]
        ++ vc.theme.main


button : Config -> List Style
button vc =
    cursor pointer :: vc.theme.button.base


primary : Config -> List Style
primary vc =
    button vc ++ vc.theme.button.primary


danger : Config -> List Style
danger vc =
    button vc ++ vc.theme.button.primary


disabled : Config -> List Style
disabled vc =
    cursor notAllowed :: vc.theme.button.disabled


tool : Config -> List Style
tool vc =
    vc.theme.tool


hovercard : Config -> List ( String, String )
hovercard vc =
    vc.theme.hovercard.root


inputRaw : Config -> List ( String, String )
inputRaw vc =
    vc.theme.inputRaw


input : Config -> List Style
input vc =
    inputRaw vc
        |> List.map
            (\( k, v ) -> property k v)


link : Config -> List Style
link vc =
    vc.theme.link


overlay : Config -> List Style
overlay vc =
    position absolute
        :: height (vh 100)
        :: width (vw 100)
        :: displayFlex
        :: justifyContent center
        :: alignItems center
        :: zIndex (int 500)
        :: vc.theme.overlay


popup : Config -> List Style
popup vc =
    vc.theme.popup


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.loadingSpinner


footer : Config -> List Style
footer vc =
    vc.theme.footer
