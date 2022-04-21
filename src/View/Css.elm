module View.Css exposing (..)

import Css exposing (..)
import View.Config exposing (Config)


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


main_ : Config -> List Style
main_ vc =
    [ flexGrow (num 1)
    ]
        ++ vc.theme.main
