module View.Pathfinder.Network exposing (addresses, relations)

import Animation
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Dict exposing (Dict)
import List.Extra
import Model.NetworkCapabilities exposing (NetworkCapabilities)
import Model.Pathfinder exposing (Hovered(..), unit)
import Model.Pathfinder.Address as Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.DetailLevel exposing (DetailLevel(..))
import Model.Pathfinder.Highlighting as Highlighting exposing (Highlighting)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.SearchBox as SearchBox
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg)
import RemoteData
import Set exposing (Set)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Annotations as Annotations
import View.Pathfinder.Address as Address
import View.Pathfinder.AggEdge as AggEdge
import View.Pathfinder.ConversionEdge as ConversionEdge
import View.Pathfinder.Tx as Tx
import View.Pathfinder.Tx.Utxo exposing (RenderLevel(..))


{-| The address nodes. Addresses the on-graph filters hide are left out
(`Highlighting.forNode`).
-}
addresses : View.Config -> Pathfinder.Config -> DetailLevel -> NetworkCapabilities -> Highlighting -> Annotations.AnnotationModel -> Dict Id Address -> Svg Msg
addresses vc pc level capabilities highlighting annotations =
    Dict.foldl
        (\id address svg ->
            case Highlighting.forNode highlighting id of
                Nothing ->
                    svg

                Just hl ->
                    ( Id.toString id
                    , Annotations.getAnnotation id annotations
                        |> Svg.lazy7 Address.view vc pc capabilities hl level address
                    )
                        :: svg
        )
        []
        >> Keyed.node "g" []


{-| The graph edges, in whichever tracing mode is active.

The two branches render a different number of keyed `g` children at the same
position in the tree, so switching mode makes elm diff them into a "remove the
last n children" patch. On the stock elm kernel that patch assumes the DOM
still looks exactly like the previous virtual DOM, and it throws
"Node.removeChild: Argument 1 is not an object" when it does not — which is
what a browser extension, or anything else reaching into the page, makes
happen. We rely on elm-safe-virtual-dom (see `virtual-dom-fix` in the Makefile)
to tolerate that; a build without it crashes on the tx -> relationship switch.

So if edges ever go stale, duplicate or fail to disappear on a mode switch,
suspect this swap before suspecting the model: the keyed children below are the
part of the view whose structure changes underneath elm.

-}
relations : View.Config -> Pathfinder.Config -> DetailLevel -> Hovered -> Selection -> Highlighting -> Annotations.AnnotationModel -> Dict Id Tx -> Dict ( Id, Id ) AggEdge -> Dict ( Id, Id ) ConversionEdge -> Svg Msg
relations vc gc level hovered selection highlighting annotations txs agg conversions =
    case gc.tracingMode of
        Pathfinder.AggregateTracingMode ->
            -- Exactly at Svg.Styled.Lazy's lazy7 limit. Adding another argument
            -- means packing the scalars into one value, or memoization is lost.
            Svg.lazy7 aggRelations vc level hovered selection highlighting txs agg

        Pathfinder.TransactionTracingMode ->
            Svg.lazy7 txRelations vc gc level highlighting annotations txs conversions


{-| Render aggregate-mode edges.

  - `level` — below `Full` the always-on mid-point value labels are omitted to
    reduce clutter when zoomed out; hovered/selected edges still show their
    label via the highlight layer.
  - `hovered`/`selection` — the hovered and selected graph elements. When an
    edge is hovered/selected or an address is hovered/selected, edges not
    incident to that focus are dimmed so the relevant edges are easy to trace.
    These are passed as the raw model values (rather than a derived list) so
    that `Svg.lazy` memoization holds: they keep a stable reference between
    renders unless the hover/selection actually changes.
  - `txs` — the transactions that exist in tx-mode. Each agg edge knows the
    set of underlying txs (`edge.txs`); the y-centroid of their resting
    positions is used as the preferred vertical position for the edge's label,
    so toggling between tx and aggregate mode keeps the same visual layout.

-}
aggRelations : View.Config -> DetailLevel -> Hovered -> Selection -> Highlighting -> Dict Id Tx -> Dict ( Id, Id ) AggEdge -> Svg Msg
aggRelations vc level hovered selection highlighting txs agg =
    let
        agg_ =
            Dict.values agg
                |> List.filter
                    (\edge ->
                        RemoteData.isSuccess edge.a2b
                            && RemoteData.isSuccess edge.b2a
                            && (Highlighting.forAggEdge highlighting ( edge.a, edge.b ) /= Nothing)
                    )

        -- hovered and/or selected addresses that drive the focus dimming
        focusAddressIds =
            (case hovered of
                HoveredAddress id ->
                    [ id ]

                _ ->
                    []
            )
                ++ (case selection of
                        SelectedAddress id ->
                            [ id ]

                        MultiSelect opts ->
                            List.filterMap
                                (\o ->
                                    case o of
                                        MSelectedAddress aid ->
                                            Just aid

                                        MSelectedTx _ ->
                                            Nothing
                                )
                                opts

                        _ ->
                            []
                   )

        -- nodes touched by a hovered/selected edge, plus the hovered/selected
        -- addresses
        focusedNodes =
            agg_
                |> List.concatMap
                    (\edge ->
                        if edge.hovered || edge.selected then
                            [ edge.a, edge.b ]

                        else
                            []
                    )
                |> (++) focusAddressIds
                |> Set.fromList

        focusActive =
            not (Set.isEmpty focusedNodes)

        isDimmed edge =
            let
                -- dimmed by the on-graph search, or a filtered-out edge
                -- shown in grey
                searchDimmed =
                    Highlighting.forAggEdge highlighting ( edge.a, edge.b ) == Just SearchBox.Dimmed

                focusDimmed =
                    focusActive
                        && not (edge.hovered || edge.selected)
                        && not (Set.member edge.a focusedNodes)
                        && not (Set.member edge.b focusedNodes)
            in
            searchDimmed || focusDimmed

        lookOf edge =
            if Highlighting.forAggEdge highlighting ( edge.a, edge.b ) == Just SearchBox.FilteredOut then
                Filtered

            else if isDimmed edge then
                Dimmed

            else
                Normal

        -- edges paired with their resolved endpoint addresses
        placedEdges =
            agg_
                |> List.filterMap
                    (\edge ->
                        Maybe.map2 (\a b -> ( edge, a, b )) edge.aAddress edge.bAddress
                    )

        -- per-edge preferred label center y (in pixels) derived from the
        -- resting positions of the underlying txs in tx-mode
        hintsByEdge =
            placedEdges
                |> List.filterMap
                    (\( edge, _, _ ) ->
                        avgTxY txs edge.txs
                            |> Maybe.map (\y -> ( ( edge.a, edge.b ), y * unit ))
                    )
                |> Dict.fromList

        -- per-edge 2D label offset: auto-placed Y plus any user-pinned x/y
        labelOffsets =
            resolveLabelOffsets vc hintsByEdge placedEdges

        offsetFor edge =
            Dict.get ( edge.a, edge.b ) labelOffsets
                |> Maybe.withDefault { x = 0, y = 0 }
    in
    Svg.g []
        [ placedEdges
            |> List.map
                (\( edge, a, b ) -> aggEdgeEdge vc (lookOf edge) (offsetFor edge) edge a b)
            |> Keyed.node "g" []
        , if level == Full then
            placedEdges
                |> List.map
                    (\( edge, a, b ) -> aggEdgeNode vc (lookOf edge) (offsetFor edge) edge a b)
                |> Keyed.node "g" []

          else
            Svg.g [] []
        , placedEdges
            |> List.filter (\( edge, _, _ ) -> edge.selected || edge.hovered)
            |> List.map
                (\( edge, a, b ) -> aggEdgeNodeHighlight vc (offsetFor edge) edge a b)
            |> Keyed.node "g" []
        ]


{-| An axis-aligned box, centered on `( x, y )`, in pixel coordinates.
-}
type alias Box =
    { x : Float, y : Float, w : Float, h : Float }


boxesOverlap : Box -> Box -> Bool
boxesOverlap a b =
    let
        margin =
            4
    in
    (abs (a.x - b.x) < (a.w + b.w) / 2 + margin)
        && (abs (a.y - b.y) < (a.h + b.h) / 2 + margin)


{-| Assign each aggregate-edge label a vertical pixel offset so that labels do
not overlap each other or the address nodes. Greedy placement: edges are
processed left-to-right, and each label takes the first collision-free
candidate offset. The candidate ladder for each label starts with the
tx-position hint (if available), then 0, then alternating up/down.
-}
resolveLabelOffsets : View.Config -> Dict ( Id, Id ) Float -> List ( AggEdge, Address, Address ) -> Dict ( Id, Id ) { x : Float, y : Float }
resolveLabelOffsets vc hints placedEdges =
    let
        nodeBox address =
            let
                c =
                    Address.getCoords address
            in
            { x = c.x * unit, y = c.y * unit, w = unit, h = unit }

        nodeObstacles =
            placedEdges
                |> List.concatMap (\( _, a, b ) -> [ nodeBox a, nodeBox b ])

        labels =
            placedEdges
                |> List.map
                    (\( edge, a, b ) ->
                        let
                            m =
                                AggEdge.labelMetrics vc edge a b

                            key =
                                ( edge.a, edge.b )

                            -- preferred *offset* from the default label y
                            preferredOff =
                                Dict.get key hints
                                    |> Maybe.map (\hy -> hy - m.y)
                        in
                        ( key, Box m.x m.y m.width m.height, ( preferredOff, edge.labelOffset ) )
                    )
                |> List.sortBy (\( _, b, _ ) -> b.x)

        step =
            labels
                |> List.head
                |> Maybe.map (\( _, b, _ ) -> b.h)
                |> Maybe.withDefault 30
                |> (*) 1.2

        defaultCandidates =
            0 :: List.concatMap (\i -> [ step * toFloat i, step * toFloat -i ]) (List.range 1 5)

        place ( key, box, ( preferredOff, manualOff ) ) ( obstacles, offsets ) =
            case manualOff of
                Just m ->
                    -- User-pinned label: keep the chosen position even if
                    -- it overlaps something. Still seeded as an obstacle so
                    -- auto-placed neighbours avoid sitting on top of it.
                    ( { box | x = box.x + m.x, y = box.y + m.y } :: obstacles
                    , Dict.insert key m offsets
                    )

                Nothing ->
                    let
                        candidates =
                            case preferredOff of
                                Just p ->
                                    p :: defaultCandidates

                                Nothing ->
                                    defaultCandidates

                        chosen =
                            candidates
                                |> List.Extra.find
                                    (\off -> not (List.any (boxesOverlap { box | y = box.y + off }) obstacles))
                                |> Maybe.withDefault 0
                    in
                    ( { box | y = box.y + chosen } :: obstacles
                    , Dict.insert key { x = 0, y = chosen } offsets
                    )
    in
    labels
        |> List.foldl place ( nodeObstacles, Dict.empty )
        |> Tuple.second


{-| Mean resting y-coordinate (in graph units) of the txs in `txIds` that are
currently part of the visible graph. `Nothing` if none of them is present.
Resting (not animated) y is used so that pan/zoom does not re-trigger work.
-}
avgTxY : Dict Id Tx -> Set Id -> Maybe Float
avgTxY txs txIds =
    let
        ys =
            txIds
                |> Set.toList
                |> List.filterMap (\id -> Dict.get id txs)
                |> List.map (\tx -> Animation.getTo tx.y)
    in
    case ys of
        [] ->
            Nothing

        _ ->
            Just (List.sum ys / toFloat (List.length ys))


txRelations : View.Config -> Pathfinder.Config -> DetailLevel -> Highlighting -> Annotations.AnnotationModel -> Dict Id Tx -> Dict ( Id, Id ) ConversionEdge -> Svg Msg
txRelations vc gc level highlighting annotations txs conversions =
    let
        -- each tx with its highlight; txs the on-graph filters hide are left out
        txs_ =
            Dict.values txs
                |> List.filterMap (\tx -> Highlighting.forNode highlighting tx.id |> Maybe.map (\hl -> ( tx, hl )))

        conversions_ =
            Dict.values conversions
                |> List.filter (\c -> Highlighting.forConversion highlighting c.id [] /= Nothing)

        ( txsHighlighted_, txsRegular_ ) =
            List.partition (\( tx, _ ) -> tx.hovered || tx.selected) txs_
    in
    (if vc.showConversionEdges then
        [ conversions_
            |> ConversionEdge.withDisplacementIndex
            |> List.map
                (\( i, ( conversion, a, b ) ) ->
                    conversionEdge vc gc highlighting conversion i a b
                )
            |> Keyed.node "g" []
        ]

     else
        []
    )
        ++ [ txsRegular_
                |> List.map
                    (\( tx, hl ) ->
                        ( Id.toString tx.id |> (++) "te"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.edge vc gc hl Edge tx
                        )
                    )
                |> Keyed.node "g" []
           , -- edge value labels drop out when zoomed out; hovered and selected
             -- txs are in the highlighted lists below and keep theirs
             (if level == Full then
                txsRegular_

              else
                []
             )
                |> List.map
                    (\( tx, hl ) ->
                        ( Id.toString tx.id |> (++) "tl"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.edge vc gc hl Label tx
                        )
                    )
                |> Keyed.node "g" []
           , txsRegular_
                |> List.map
                    (\( tx, hl ) ->
                        ( Id.toString tx.id |> (++) "tn"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.view vc gc hl level tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\( tx, hl ) ->
                        ( Id.toString tx.id |> (++) "teh"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.edge vc gc hl Edge tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\( tx, hl ) ->
                        ( Id.toString tx.id |> (++) "tlh"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.edge vc gc hl Label tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\( tx, hl ) ->
                        ( Id.toString tx.id |> (++) "tnh"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.view vc gc hl level tx
                        )
                    )
                |> Keyed.node "g" []
           ]
        |> Svg.g []


{-| Opacity for an aggregate edge: reduced when dimmed for focus or on-graph
search. The opacity is always set (not only when dimmed) and carries a CSS
transition, so focus changes fade smoothly instead of snapping — this avoids
the flicker that hard toggling caused when the mouse moves across the graph.
-}
dimAttrs : EdgeLook -> List (Svg.Attribute Msg)
dimAttrs look =
    [ css
        ([ Css.opacity
            (Css.num
                (case look of
                    Dimmed ->
                        0.15

                    Filtered ->
                        0.35

                    Normal ->
                        1
                )
            )
         , Css.property "transition" "opacity 0.2s ease-in-out"
         ]
            -- one css value: two would each become a class, and which wins
            -- would depend on how their text sorts (see .claude/CLAUDE.md).
            -- Grey only, not dashed: dashes on parallel edges read as stripes
            -- (2026-11-27)
            ++ (if look == Filtered then
                    [ Css.property "filter" "grayscale(1)" ]

                else
                    []
               )
        )
    ]


{-| How an aggregated edge is drawn: dimmed by focus or search, or hidden by
the filters and drawn grey and dashed because grey mode is on.
-}
type EdgeLook
    = Normal
    | Dimmed
    | Filtered


aggEdgeNodeHighlight : View.Config -> { x : Float, y : Float } -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNodeHighlight vc offset edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "eh"
    , Svg.lazy5 AggEdge.highlight vc offset edge aAddress bAddress
    )


aggEdgeNode : View.Config -> EdgeLook -> { x : Float, y : Float } -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNode vc look offset edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "en"
    , Svg.g
        (dimAttrs look)
        [ Svg.lazy5 AggEdge.view vc offset edge aAddress bAddress ]
    )


aggEdgeEdge : View.Config -> EdgeLook -> { x : Float, y : Float } -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeEdge vc look offset edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "ee"
    , Svg.g
        (dimAttrs look)
        [ Svg.lazy6 AggEdge.edge vc offset edge aAddress bAddress False ]
    )


conversionEdge : View.Config -> Pathfinder.Config -> Highlighting -> ConversionEdge -> Int -> Address -> Address -> ( String, Svg Msg )
conversionEdge vc _ highlighting conversion displacementIndex aAddress bAddress =
    let
        ( inputTxId, outputTxId ) =
            conversion.id
    in
    ( ConversionEdge.toIdString conversion |> (++) "ce"
    , Svg.lazy6 ConversionEdge.view
        vc
        (Highlighting.forConversion highlighting conversion.id [ aAddress.id, bAddress.id, inputTxId, outputTxId ]
            |> Maybe.withDefault SearchBox.Dimmed
        )
        conversion
        displacementIndex
        aAddress
        bAddress
    )



{-
   txs : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> List Tx -> Svg Msg
   txs plugins vc gc annotations =
       List.foldl
           (\tx svg ->
               ( Id.bString tx.id
               , Annotations.getAnnotation tx.id annotations
                   |> Svg.lazy5 Tx.view plugins vc gc tx
               )
                   :: svg
           )
           []
           >> Keyed.node "g" []
-}
