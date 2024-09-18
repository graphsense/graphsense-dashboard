module Config.Pathfinder exposing (..)

import Hovercard


addressRadius : Float
addressRadius =
    1


nodeXOffset : Float
nodeXOffset =
    4


nodeYOffset : Float
nodeYOffset =
    2


type alias Config =
    { displaySettingsHovercard : Maybe Hovercard.Model, isClusterDetailsOpen : Bool, displayAllTagsInDetails : Bool, snapToGrid : Bool }
