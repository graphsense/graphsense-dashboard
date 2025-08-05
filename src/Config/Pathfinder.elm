module Config.Pathfinder exposing (Config, TracingMode(..), addressRadius, nodeXOffset, nodeYOffset)


addressRadius : Float
addressRadius =
    1


nodeXOffset : Float
nodeXOffset =
    4


nodeYOffset : Float
nodeYOffset =
    2.5


type TracingMode
    = TransactionTracingMode
    | AggregateTracingMode


type alias Config =
    { snapToGrid : Bool
    , highlightClusterFriends : Bool
    , tracingMode : TracingMode
    }
