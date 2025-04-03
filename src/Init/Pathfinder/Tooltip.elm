module Init.Pathfinder.Tooltip exposing (init)

import Hovercard
import Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType)


init : Hovercard.Model -> TooltipType msg -> Tooltip msg
init hc tt =
    Tooltip hc tt False
