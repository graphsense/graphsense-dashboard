module View.Css exposing (..)

import Css exposing (..)
import View.Config exposing (Config)


heading2 : Config -> List Style
heading2 vc =
    [ vc.theme.heading2
    ]


main_ : Config -> List Style
main_ vc =
    [ flexGrow (num 1)
    , vc.theme.main
    ]
