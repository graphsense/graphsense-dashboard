module Css.Graph exposing (svgRoot)

import Config.View exposing (Config)
import Css exposing (..)


svgRoot : Config -> List Style
svgRoot vc =
    [ pct 100 |> width
    , property "color" "black"
    , property "user-select" "none"
    ]
        ++ vc.theme.graph.svgRoot vc.lightmode
