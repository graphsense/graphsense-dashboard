module Css.View exposing (..)

import Config.View exposing (Config)
import Css exposing (..)
import Util.Css


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
        ++ vc.theme.body vc.lightmode


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
        :: Util.Css.zIndexMain
        :: flexDirection column
        :: vc.theme.sidebar vc.lightmode


sidebarIcon : Config -> Bool -> List Style
sidebarIcon vc active =
    vc.theme.sidebarIcon vc.lightmode active


main_ : Config -> List Style
main_ vc =
    [ flexGrow (num 1)
    ]
        ++ vc.theme.main vc.lightmode


button : Config -> List Style
button vc =
    cursor pointer :: vc.theme.button.button vc.lightmode


primary : Config -> List Style
primary vc =
    button vc ++ vc.theme.button.primary vc.lightmode


danger : Config -> List Style
danger vc =
    button vc ++ vc.theme.button.danger vc.lightmode


disabled : Config -> List Style
disabled vc =
    cursor notAllowed :: vc.theme.button.disabled vc.lightmode


tool : Config -> List Style
tool vc =
    vc.theme.tool


hovercard : Config -> List ( String, String )
hovercard vc =
    (vc.theme.hovercard vc.lightmode).root


inputRawWithLength : Config -> Float -> List ( String, String )
inputRawWithLength vc px =
    vc.theme.inputRaw vc.lightmode (Just px)


inputRaw : Config -> List ( String, String )
inputRaw vc =
    vc.theme.inputRaw vc.lightmode Nothing


input : Config -> List Style
input vc =
    inputRaw vc
        |> List.map
            (\( k, v ) -> property k v)


link : Config -> List Style
link vc =
    cursor pointer
        :: vc.theme.link vc.lightmode


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
    vc.theme.popup vc.lightmode


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.loadingSpinner


footer : Config -> List Style
footer vc =
    position absolute
        :: bottom (px 0)
        :: width (pct 100)
        :: Util.Css.zIndexMain
        :: vc.theme.footer


buttonsRow : Config -> List Style
buttonsRow vc =
    displayFlex
        :: justifyContent spaceBetween
        :: vc.theme.buttonsRow


switchLabel : Config -> List Style
switchLabel vc =
    vc.theme.switchLabel


switchRoot : Config -> List Style
switchRoot vc =
    vc.theme.switchRoot
