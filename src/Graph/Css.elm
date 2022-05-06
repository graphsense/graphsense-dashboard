module Graph.Css exposing (..)

import Css exposing (..)
import View.Config exposing (Config)


root : Config -> List Style
root vc =
    [ displayFlex
    , flexDirection column
    , pct 100 |> height
    ]
        ++ vc.theme.graph.root


navbar : Config -> List Style
navbar vc =
    [ zIndex <| int 50
    , displayFlex
    , flexDirection row
    ]
        ++ vc.theme.graph.navbar


navbarLeft : Config -> List Style
navbarLeft vc =
    [ displayFlex
    , flexDirection row
    , alignItems center
    , justifyContent flexStart
    ]
        ++ vc.theme.graph.navbarLeft


navbarRight : Config -> List Style
navbarRight vc =
    [ displayFlex
    , flexDirection row
    , alignItems center
    , flexGrow <| num 1
    , justifyContent flexEnd
    ]
        ++ vc.theme.graph.navbarRight


tool : Config -> List Style
tool vc =
    vc.theme.graph.tool


graphRoot : Config -> List Style
graphRoot vc =
    [ pct 100 |> height
    , overflow hidden
    , position relative
    ]
        ++ vc.theme.graph.graphRoot


addressRoot : Config -> List Style
addressRoot vc =
    vc.theme.graph.addressRoot


addressRect : Config -> List Style
addressRect vc =
    vc.theme.graph.addressRect


addressFlags : Config -> List Style
addressFlags vc =
    vc.theme.graph.addressFlags


addressLabel : Config -> List Style
addressLabel vc =
    vc.theme.graph.addressLabel
