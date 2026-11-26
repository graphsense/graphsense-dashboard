module Msg.Pathfinder.GraphFilter exposing (Msg(..))

import Model.Pathfinder.GraphFilter exposing (Period)


type Msg
    = UserClickedToggleFilter
    | UserClickedCloseFilter
    | UserToggledHideTiny
    | UserClickedChangeLimit
    | UserClickedLimitDone
    | UserChangedLimit Float
    | UserToggledAsset String
    | UserClickedClearAssets
    | UserChangedPeriod (Maybe Period)
    | UserToggledShowHidden
    | UserClickedReset
