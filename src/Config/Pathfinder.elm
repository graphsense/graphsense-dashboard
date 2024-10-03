module Config.Pathfinder exposing (Config, addressRadius, nodeXOffset, nodeYOffset)


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
    { isClusterDetailsOpen : Bool, displayAllTagsInDetails : Bool, snapToGrid : Bool }
