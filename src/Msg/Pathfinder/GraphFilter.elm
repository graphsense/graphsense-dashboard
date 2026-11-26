module Msg.Pathfinder.GraphFilter exposing (Msg(..))

import Model.Pathfinder.GraphFilter exposing (Period)


type Msg
    = UserToggledHideTiny
    | UserChangedLimit Float
    | UserToggledAsset String
    | UserChangedPeriod (Maybe Period)
    | UserToggledShowHidden
    | UserClickedReset
