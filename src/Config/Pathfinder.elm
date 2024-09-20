module Config.Pathfinder exposing (Config, addressRadius, nodeXOffset, nodeYOffset)

import Hovercard


addressRadius : Float
addressRadius =
    1


nodeXOffset : Float
nodeXOffset =
    4


nodeYOffset : Float
nodeYOffset =
    2.5


type alias Config =
    { displaySettingsHovercard : Maybe Hovercard.Model, isClusterDetailsOpen : Bool, displayAllTagsInDetails : Bool, snapToGrid : Bool }
