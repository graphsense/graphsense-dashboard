module Model.Statusbar exposing (Model, getMessage, loadingActorKey, loadingActorTagsKey, loadingAddressEntityKey, loadingAddressKey, loadingTransactionKey, searchNeighborsKey)

import Dict exposing (Dict)
import Http


searchNeighborsKey : String
searchNeighborsKey =
    "Statusbar-search-parameters"


loadingAddressKey : String
loadingAddressKey =
    "Statusbar-loading-address"


loadingTransactionKey : String
loadingTransactionKey =
    "Statusbar-loading-transaction"


loadingActorKey : String
loadingActorKey =
    "Statusbar-loading-actor"


loadingActorTagsKey : String
loadingActorTagsKey =
    "Statusbar-loading-tags-of-actor"


loadingAddressEntityKey : String
loadingAddressEntityKey =
    "Statusbar-loading-entity-for-address"


{-| Statusbar state.

`retries` tracks in-flight retry attempts for API requests that failed with a
transient error (network drop, timeout, 5xx). The key is the same id used in
`messages` — the per-request statusbar token produced by `Api.effectToTracker`
(or the message-key fallback). The value is the attempt number (1..maxApiRetries)
currently scheduled or running.

It serves two purposes:

1.  **Staleness check.** When a delayed `BrowserRetryApiEffect` fires, the
    handler compares its attempt number against `Dict.get key retries`. If
    the entry is gone (request was cancelled or already succeeded) or the
    numbers disagree (a newer attempt has superseded it), the retry is
    silently dropped. This keeps retries in sync with tracker-based
    cancellation even though the HTTP layer itself is unchanged.

2.  **UI visibility.** `View.Statusbar` reads this dict to render a
    "(retrying N/max)" suffix next to the loading message, so the user can
    see that a flaky request is being recovered rather than stalled.

Entries are added in `Update.elm` when a transient failure is observed and
removed by `Update.Statusbar.update` once a final result (success or
non-transient error) arrives for that key.

-}
type alias Model =
    { messages : Dict String ( String, List String )
    , retries : Dict String Int
    , log : List ( String, List String, Maybe Http.Error )
    , visible : Bool
    , lastBlocks : List ( String, Int )
    }


getMessage : String -> Model -> Maybe ( String, List String )
getMessage key { messages } =
    Dict.get key messages
