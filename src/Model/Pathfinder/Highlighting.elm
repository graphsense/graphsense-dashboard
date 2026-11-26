module Model.Pathfinder.Highlighting exposing
    ( Highlighting
    , forAggEdge
    , forConversion
    , forNode
    , init
    )

{-| What the graph layers need to know about the on-graph search and the
on-graph filters, in one value.

It is derived state: `Update.Pathfinder` rebuilds it after every message and
replaces it only when it changed, so the lazily rendered layers
(`View.Pathfinder.Network`) see a stable reference and keep their
memoization. Those layers are at `Svg.lazy7`'s limit, which is why search and
filter travel together instead of as two arguments.

A `Nothing` from the functions below means: do not draw it.

-}

import Model.Pathfinder.GraphFilter as GraphFilter
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.SearchBox as SearchBox exposing (Highlight(..))
import Set


type alias Highlighting =
    { search : SearchBox.Model
    , filter : GraphFilter.Result
    , showHidden : Bool
    }


init : Highlighting
init =
    { search = SearchBox.empty
    , filter = GraphFilter.emptyResult
    , showHidden = False
    }


{-| An address or a transaction.
-}
forNode : Highlighting -> Id -> Maybe Highlight
forNode h id =
    if Set.member id h.filter.hiddenAddresses || Set.member id h.filter.hiddenTxs then
        hidden h

    else if Set.member id h.filter.greyedAddresses && h.showHidden then
        -- kept although the filters would remove it (starting point,
        -- annotated, selected): drawn normally, dimmed only in grey mode
        Just Dimmed

    else
        Just (SearchBox.highlightFor h.search id)


{-| A swap edge, keyed by its legs; `ids` are the ids the search matches on.
-}
forConversion : Highlighting -> ( Id, Id ) -> List Id -> Maybe Highlight
forConversion h key ids =
    if Set.member key h.filter.hiddenConversions then
        hidden h

    else
        Just (SearchBox.highlightForAny h.search ids)


{-| An edge of the aggregated tracing mode.
-}
forAggEdge : Highlighting -> ( Id, Id ) -> Maybe Highlight
forAggEdge h ( a, b ) =
    if Set.member ( a, b ) h.filter.hiddenAggEdges then
        hidden h

    else
        Just (SearchBox.highlightForAny h.search [ a, b ])


hidden : Highlighting -> Maybe Highlight
hidden h =
    if h.showHidden then
        Just Dimmed

    else
        Nothing
