module Msg.Pathfinder.SearchBox exposing (Msg(..))


type Msg
    = UserChangedQuery String
    | UserClickedNext
    | UserClickedPrev
    | UserClickedClose
    | UserPressedEnterInBox
