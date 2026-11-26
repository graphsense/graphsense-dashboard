module Msg.Pathfinder.GraphFilter exposing (Msg(..))

import Model.Pathfinder.GraphFilter exposing (Period, TimeMode)


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
    | UserChoseTimeMode TimeMode
      -- date fields, as "2026-03-02"
    | UserChangedDay String
    | UserChangedFrom String
    | UserChangedUntil String
      -- the activity strip: press, drag over and release on its buckets
    | UserPressedStripBucket Period
    | UserEnteredStripBucket Period
    | UserReleasedStripBucket Period
    | UserLeftStrip
    | UserToggledShowHidden
    | UserClickedReset
