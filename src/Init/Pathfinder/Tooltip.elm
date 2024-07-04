module Init.Pathfinder.Tooltip exposing (..)

import Hovercard
import Model.Pathfinder.Tooltip exposing (TooltipType)
import Model.Pathfinder.Tooltip exposing (Tooltip)


init : Hovercard.Model -> TooltipType -> Tooltip
init =
    Tooltip
