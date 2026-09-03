module Model.Pathfinder.DetailLevel exposing (DetailLevel(..), forNode, fromZoom)

{-| How much the graph draws per node and edge, derived from the zoom level.

Text is unreadable once zoomed out far enough, and on a large graph it only adds
noise; so labels drop out in steps. The level is a bucketed value on purpose:
the graph layers are rendered lazily, and a value that only changes when a
threshold is crossed keeps them memoized across every zoom tick in between.

-}


type DetailLevel
    = Full
    | Reduced -- no edge values, timestamps, tx hashes, tag icons
    | Minimal -- additionally no address identifiers; annotations and service labels stay


{-| Larger z means further zoomed out (0.1 .. 14). The first threshold is the
one relationship mode has used for its edge labels all along.
-}
fromZoom : Float -> DetailLevel
fromZoom z =
    if z <= 2.5 then
        Full

    else if z <= 6 then
        Reduced

    else
        Minimal


{-| A selected (or, for txs, hovered) node is what the user is looking at, so
it keeps every label whatever the zoom. `focused` says whether this node is.
-}
forNode : Bool -> DetailLevel -> DetailLevel
forNode focused level =
    if focused then
        Full

    else
        level
