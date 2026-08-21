module Css.View exposing (body, contents, copyIcon, footer, frame, heading2, hovercard, iconLink, input, link, listItem, loadingSpinner, longIdentifier, main_, navbar, overlay, paragraph, sectionBelowHeader, switchLabel, switchRoot)

import Config.View exposing (Config)
import Css exposing (..)
import Model.Dialog exposing (Placement(..))
import Theme.Colors as Colors
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


paragraph : Config -> List Style
paragraph vc =
    vc.theme.paragraph


listItem : Config -> List Style
listItem vc =
    vc.theme.listItem


sectionBelowHeader : Config -> List Style
sectionBelowHeader vc =
    [ displayFlex
    , flexDirection row
    , flexGrow (num 1)
    , alignItems stretch
    , Css.property "background-color" Colors.greyBlue20
    ]
        ++ vc.theme.sectionBelowHeader


main_ : Config -> List Style
main_ _ =
    [ flexGrow (num 1)
    , displayFlex
    , flexDirection column
    , position relative
    ]


navbar : Config -> List Style
navbar vc =
    Util.Css.zIndexMain
        :: vc.theme.navbar vc.lightmode


contents : Config -> List Style
contents vc =
    [ displayFlex
    , flexDirection column
    , flexGrow (num 1)
    , overflow auto
    ]
        ++ (vc.size
                |> Maybe.map
                    (\{ height } ->
                        px height
                            |> maxHeight
                            |> List.singleton
                    )
                |> Maybe.withDefault []
           )
        ++ vc.theme.contents vc.lightmode


hovercard : Config -> List ( String, String )
hovercard vc =
    (vc.theme.hovercard vc.lightmode).root


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


iconLink : Config -> List Style
iconLink vc =
    cursor pointer
        :: vc.theme.iconLink vc.lightmode


overlay : Placement -> Config -> List Style
overlay placement vc =
    let
        placementStyles =
            case placement of
                Centered ->
                    [ alignItems center ]

                PinnedToTop ->
                    [ alignItems flexStart
                    , paddingTop (vh 10)
                    , boxSizing borderBox
                    ]
    in
    position absolute
        :: height (vh 100)
        :: width (vw 100)
        :: displayFlex
        :: justifyContent center
        :: zIndex (int 500)
        :: placementStyles
        ++ vc.theme.overlay


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


switchLabel : Config -> List Style
switchLabel vc =
    vc.theme.switchLabel


switchRoot : Config -> List Style
switchRoot vc =
    vc.theme.switchRoot


copyIcon : Config -> List Style
copyIcon vc =
    cursor pointer
        :: vc.theme.copyIcon vc.lightmode


longIdentifier : Config -> List Style
longIdentifier vc =
    display inlineBlock :: vc.theme.longIdentifier


frame : Config -> List Style
frame vc =
    vc.theme.frame vc.lightmode
