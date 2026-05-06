module Update.Pathfinder.WorkflowNextUtxoTx exposing (Config, Error(..), Msg, Result, Workflow, start, update)

{-| Chain-walking auto-extension for UTXO transactions in the Pathfinder
graph view.

When the user clicks the "expand backward" or "expand forward" handle on
an address node, this workflow takes the most recent tx adjacent to that
address (in the opposite direction — the user's _known_ tx becomes the
starting point) and walks the spending chain one hop at a time, looking
for the next tx the user should actually see on the graph. Intermediate
txs that don't carry value across a real boundary are skipped so the
graph doesn't get cluttered with a long tail of self-shuffles or
peel-back steps.


# Where it sits

  - **Entry**: `Update.Pathfinder.expandAddress` constructs a `Config` and
    calls [`start`](#start) with the start tx, then ships the resulting
    [`Workflow`](#Workflow) effects back through the Pathfinder update
    loop.
  - **Per-step**: each API response is fed to [`update`](#update), which
    either dispatches more API calls, returns a tx the user should see
    (`Workflow.Ok`), or signals a terminal failure (`Workflow.Err`).
  - **Caller-side handling**: `Update.Pathfinder.handleWorkflowNextUtxo`
    interprets each outcome — adds the result tx to the graph, emits
    statusbar log entries on skip-counts and hop-limit hits, and clears
    the address's `TxsLoading` state.


# Algorithm

The workflow is a single recursion driven by the [`Trail`](#Trail) record
threaded through messages.

1.  [`continueWorkflow`](#continueWorkflow) on the current tx picks one
    or more index(es) on the side we'd follow into:
      - **Backward (Incoming)**: anchor's input(s) → fetch the tx that
        funded each via `ListSpendingTxRefsEffect`.
      - **Forward (Outgoing)**: anchor's output(s) → fetch the tx that
        spent each via `ListSpentInTxRefsEffect`.
2.  For each fetched candidate tx,
    [`isInternalContinuation`](#isInternalContinuation) decides whether
    the chain continues _through_ it (skip, recurse) or _stops on_ it
    (success, surface to user).
3.  Termination:
      - Predicate False → `Workflow.Ok { tx, skippedCount }`. Caller adds
        the tx to the graph.
      - Predicate True and we've hit the 50-hop limit →
        `Workflow.Err (MaxChangeHopsLimit n tx)`. Caller stores the last
        seen tx so a follow-up click resumes from there.
      - API returns no refs and we've already skipped at least one tx →
        `Workflow.Ok` on the deepest tx we walked through (using
        `lastSkipped` from the trail). The chain effectively ends there.
      - API returns no refs at the very first hop → `Workflow.Err
        NoTxFound`. Caller shows a "no adjacent tx" toast.


# Skip predicate

The decision is structural and direction-aware:

  - **Forward**: skip iff every output is anchor-only — value never left
    the anchor. Peeling txs with a real side payment are NOT skipped;
    the side payment is the value flow we want to show.
  - **Backward**: skip iff every input is anchor-only — value came from
    the anchor's own prior self-spend. Backward we want to keep walking
    until we find a non-self funding source.

Pure self-txs (anchor only on both sides) match either branch and are
skipped regardless of direction. They also happen to render badly,
because `Init.Pathfinder.Tx` filters an address out of `outputs` when
it's also in `inputs`, so the graph would draw the loop with one
missing leg.


# Picking among multiple candidates

When the current tx has more than one input (backward) or output
(forward) involving the anchor, [`Config.allowMultiple`](#Config)
controls fan-out:

  - `True`: follow every matching index — multiple HTTP requests in
    parallel, each producing its own continuation chain. Used when the
    caller wants every reachable next tx.
  - `False` (typical): pick exactly one. Forward prefers outputs the
    consensus change-heuristic did NOT flag as change (i.e. the "real"
    value flow), then biggest by value; backward picks biggest by value.

-}

import Api.Data
import Effect.Api as Api
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id as Id exposing (Id)
import Workflow


{-| Hard cap on the number of hops the workflow will walk before giving
up and surfacing `MaxChangeHopsLimit`. Prevents pathological peel-chains
from chewing through hundreds of API requests; the user can click
expand again to resume past the limit.
-}
maxHops : Int
maxHops =
    50


{-| Workflow inputs.

  - `addressId` — the anchor address the user is expanding.
  - `direction` — `Outgoing` for forward (where did the anchor's value
    go?), `Incoming` for backward (where did the anchor's value come
    from?).
  - `allowMultiple` — fan out to every matching index instead of picking
    one. See module docs § "Picking among multiple candidates".

-}
type alias Config =
    { addressId : Id
    , direction : Direction
    , allowMultiple : Bool
    }


{-| Successful outcome of the workflow.

  - `tx` — the tx the user lands on; the caller adds this to the graph.
  - `skippedCount` — number of intermediate txs auto-extended past on
    the way to `tx`. Surfaced to the user as a statusbar log line
    ("Auto-extended through N skipped change/self transaction(s) …")
    when greater than zero. Informational; not used for layout.

-}
type alias Result =
    { tx : Api.Data.TxUtxo
    , skippedCount : Int
    }


{-| Workflow failure outcomes.

  - `NoTxFound` — the chain genuinely dead-ends at the first hop (the
    output is unspent, or the anchor isn't on the side we'd follow).
    Caller surfaces a toast and clears the address's tx-loading state.
  - `MaxChangeHopsLimit n lastTx` — walked `n` hops without finding a
    non-skippable tx. Caller stores `lastTx` as `TxsLastCheckedChangeTx`
    so the user can click expand again to resume from there.

-}
type Error
    = NoTxFound
    | MaxChangeHopsLimit Int Api.Data.TxUtxo


{-| State threaded through workflow messages.

  - `hops` — number of API round-trips so far; gates the
    `MaxChangeHopsLimit` cutoff.
  - `lastSkipped` — most recently auto-extended-past tx, or `Nothing`
    before the first skip. When refs come back empty mid-chain, this
    lets us fall back to "the deepest tx we walked through" as a
    successful landing instead of erroring with `NoTxFound`.

-}
type alias Trail =
    { hops : Int
    , lastSkipped : Maybe Api.Data.TxUtxo
    }


{-| Internal workflow messages dispatched and consumed by [`update`](#update).
Wrapped by the caller into top-level Pathfinder messages; not meant to be
pattern-matched outside this module.
-}
type Msg
    = BrowserGotReferencedTxs Trail (List Api.Data.TxRef)
    | BrowserGotTxForReferencedTx Trail Api.Data.Tx


type alias Workflow =
    Workflow.Workflow Result Msg Error


{-| Begin a new auto-extension walk from `tx` for the anchor in `config`.
The caller provides `tx` — typically the most recent tx adjacent to the
anchor in the _opposite_ direction (because that's the user's known
starting point on the graph) — and this kicks off the first hop.

Returns the workflow's first batch of API effects, or a terminal error
immediately if there's no leg to follow on the start tx.

-}
start : Config -> Api.Data.TxUtxo -> Workflow
start =
    continueWorkflow { hops = 0, lastSkipped = Nothing }


{-| Drive one step of the workflow forward in response to an arriving
API result. Called by `Update.Pathfinder` whenever a `WorkflowNextUtxoTx`
message comes in.

Two message shapes:

  - `BrowserGotReferencedTxs` — the list-refs API returned. If empty,
    terminate (using `lastSkipped` as a fallback landing if available);
    otherwise fan out into one `GetTxEffect` per ref.
  - `BrowserGotTxForReferencedTx` — a fetched tx body arrived. Run
    [`isInternalContinuation`](#isInternalContinuation):
      - skip → recurse via `continueWorkflow`, increment hop counter,
        record this tx as `lastSkipped`. If we'd exceed `maxHops`,
        return `MaxChangeHopsLimit` instead.
      - don't skip → land on this tx as `Workflow.Ok`.

Account-model txs (`TxTxAccount`) shouldn't normally appear in this
workflow (it's UTXO-only); if one does, terminate with `NoTxFound`.

-}
update : Config -> Msg -> Workflow
update config msg =
    case msg of
        BrowserGotReferencedTxs trail refs ->
            if List.isEmpty refs then
                case trail.lastSkipped of
                    Just tx ->
                        -- We walked at least one peeling step, then the chain
                        -- ran out of further spending. Land on the last skipped
                        -- tx (the deepest reached) so the user sees where the
                        -- chain effectively ends rather than an empty toast.
                        Workflow.Ok
                            { tx = tx
                            , skippedCount = max 0 (trail.hops - 1)
                            }

                    Nothing ->
                        Workflow.Err NoTxFound

            else
                refs
                    |> List.map
                        (\ref ->
                            BrowserGotTxForReferencedTx trail
                                |> Api.GetTxEffect
                                    { currency = Id.network config.addressId
                                    , txHash = ref.txHash
                                    , includeIo = True
                                    , tokenTxId = Nothing
                                    }
                        )
                    |> Workflow.Next

        BrowserGotTxForReferencedTx trail (Api.Data.TxTxUtxo tx) ->
            if isInternalContinuation config tx then
                if trail.hops > maxHops then
                    Workflow.Err (MaxChangeHopsLimit maxHops tx)

                else
                    continueWorkflow
                        { hops = trail.hops, lastSkipped = Just tx }
                        config
                        tx

            else
                Workflow.Ok
                    { tx = tx
                    , skippedCount = trail.hops
                    }

        BrowserGotTxForReferencedTx _ (Api.Data.TxTxAccount _) ->
            Workflow.Err NoTxFound


{-| Skip iff the anchor's value isn't actually crossing a boundary in this
tx — there's no other party on the side we'd follow into.

  - **Forward (Outgoing)**: skip if anchor's outputs are all back to the
    anchor (no real recipient). The value didn't leave the anchor.
  - **Backward (Incoming)**: skip if the anchor's inputs are all from the
    anchor (no real source). The value came from the anchor's own prior
    self-spend; walk further back to find a non-self predecessor.

In both cases we also require the anchor to be present on the opposite
side, but that's true by construction of how the workflow reaches the tx.

Pure self-txs (anchor only, both sides) match either branch and get
skipped regardless of direction. Peeling txs with a real side payment
(anchor on both sides + another recipient) are NOT skipped on forward —
the side payment is the value flow we want to show — and similarly,
backward txs with a non-anchor input are NOT skipped — that's the funding
source the user is looking for.

-}
isInternalContinuation : Config -> Api.Data.TxUtxo -> Bool
isInternalContinuation config tx =
    let
        anchor =
            Id.id config.addressId

        inputs =
            Maybe.withDefault [] tx.inputs

        outputs =
            Maybe.withDefault [] tx.outputs

        anchorOn values =
            List.any (.address >> List.member anchor) values

        allAnchorOnly values =
            not (List.isEmpty values)
                && List.all (\v -> List.all ((==) anchor) v.address) values
    in
    case config.direction of
        Outgoing ->
            anchorOn inputs && allAnchorOnly outputs

        Incoming ->
            anchorOn outputs && allAnchorOnly inputs


{-| Does the GraphSense change-heuristic consensus list flag this output as
change? Match by output index when present (the canonical key); fall back
to address membership otherwise. Only consulted by the forward picker —
the skip predicate doesn't use this anymore.
-}
isConsensusChangeOutput : List Api.Data.ConsensusEntry -> Api.Data.TxValue -> Bool
isConsensusChangeOutput consensusEntries output =
    let
        byAddress =
            List.Extra.find (\entry -> List.member entry.output.address output.address) consensusEntries
    in
    case output.index of
        Just outputIndex ->
            case List.Extra.find (\entry -> entry.output.index == outputIndex) consensusEntries of
                Just _ ->
                    True

                Nothing ->
                    byAddress /= Nothing

        Nothing ->
            byAddress /= Nothing


{-| Issue the next hop's API request(s).

Picks index(es) on the side we'd follow (inputs for backward, outputs for
forward) using direction-specific helpers. If no index matches — meaning
the anchor isn't on the relevant side of `tx` — terminate: land on
`lastSkipped` if we have one, otherwise `NoTxFound`. This guard prevents
the workflow from emitting an empty effect list that would silently stall
the loading state.

The hop counter is incremented here, on dispatch (not on response), so a
fan-out via `allowMultiple = True` still counts as one logical hop.

-}
continueWorkflow : Trail -> Config -> Api.Data.TxUtxo -> Workflow
continueWorkflow trail config tx =
    let
        ( listLinkedTxRefs, indices ) =
            case config.direction of
                Incoming ->
                    ( Api.ListSpendingTxRefsEffect
                    , findOwnAddressIoIndex config tx.inputs
                    )

                Outgoing ->
                    ( Api.ListSpentInTxRefsEffect
                    , findOutgoingContinuationIndex config tx
                    )
    in
    if List.isEmpty indices then
        -- No leg to follow from `tx`. If we already skipped at least one tx,
        -- land on it; otherwise this is a true dead-end at start and we
        -- surface NoTxFound.
        case trail.lastSkipped of
            Just last ->
                Workflow.Ok
                    { tx = last
                    , skippedCount = max 0 (trail.hops - 1)
                    }

            Nothing ->
                Workflow.Err NoTxFound

    else
        indices
            |> List.map
                (\index ->
                    BrowserGotReferencedTxs { trail | hops = trail.hops + 1 }
                        |> listLinkedTxRefs
                            { currency = tx.currency
                            , txHash = tx.txHash
                            , index = Just index
                            }
                )
            |> Workflow.Next


{-| Backward picker. Among the input values that include the anchor, pick
which index(es) to follow back via `ListSpendingTxRefsEffect`.

  - `allowMultiple = True`: every matching input.
  - `allowMultiple = False`: the biggest by value (the "main" funding leg).

No consensus heuristic here — change classification only applies to
outputs. Returns the raw `index` field where available, falling back to
the positional index in the list.

-}
findOwnAddressIoIndex : Config -> Maybe (List Api.Data.TxValue) -> List Int
findOwnAddressIoIndex { addressId, allowMultiple } values =
    let
        anchor =
            Id.id addressId

        matchesAnchor =
            .address >> List.member anchor
    in
    values
        |> Maybe.withDefault []
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.second >> matchesAnchor)
        |> (if allowMultiple then
                List.map (\( pos, v ) -> v.index |> Maybe.withDefault pos)

            else
                -- Pick the biggest input from the anchor by value. No consensus
                -- to consider on the input side.
                List.Extra.maximumBy (Tuple.second >> .value >> .value)
                    >> Maybe.map (\( pos, v ) -> [ v.index |> Maybe.withDefault pos ])
                    >> Maybe.withDefault []
           )


{-| Forward picker. Among the outputs that go to the anchor, pick which
index(es) to follow via `ListSpentInTxRefsEffect`.

Selection prefers outputs the consensus change-heuristic did NOT flag as
change — those represent the "real" continuation of the value flow,
distinct from change peeled back to self. If every anchor-output is
flagged as change (typical pure peel-chain step), fall back to the full
set.

  - `allowMultiple = True`: every output in the chosen pool.
  - `allowMultiple = False`: the biggest by value within the pool.

-}
findOutgoingContinuationIndex : Config -> Api.Data.TxUtxo -> List Int
findOutgoingContinuationIndex { addressId, allowMultiple } tx =
    let
        anchor =
            Id.id addressId

        consensus =
            tx.heuristics
                |> Maybe.andThen .changeHeuristics
                |> Maybe.map .consensus
                |> Maybe.withDefault []

        outputsToAnchor =
            tx.outputs
                |> Maybe.withDefault []
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> .address >> List.member anchor)

        nonChange =
            outputsToAnchor
                |> List.filter (Tuple.second >> isConsensusChangeOutput consensus >> not)

        candidatePool =
            -- Prefer outputs to anchor that consensus did NOT flag as change
            -- (a "real" continuation of the value flow). If none qualify, fall
            -- back to all outputs to anchor.
            if List.isEmpty nonChange then
                outputsToAnchor

            else
                nonChange

        toIndex ( pos, v ) =
            v.index |> Maybe.withDefault pos
    in
    if allowMultiple then
        candidatePool |> List.map toIndex

    else
        candidatePool
            |> List.Extra.maximumBy (Tuple.second >> .value >> .value)
            |> Maybe.map (toIndex >> List.singleton)
            |> Maybe.withDefault []
