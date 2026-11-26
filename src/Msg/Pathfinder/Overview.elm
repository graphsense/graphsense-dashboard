module Msg.Pathfinder.Overview exposing (Msg(..))

{-| Points are in graph pixels; the view converts from map pixels.
-}


type Msg
    = UserPressedOverview { x : Float, y : Float }
    | UserDraggedOverview { x : Float, y : Float }
    | UserReleasedOverview
    | UserClickedHideOverview
    | UserClickedShowOverview
