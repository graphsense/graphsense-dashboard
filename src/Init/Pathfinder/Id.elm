module Init.Pathfinder.Id exposing (init, initClusterId, initClusterIdFromAddress, initClusterIdFromRecord, initFromRecord)

import Api.Data
import Char
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (pair)
import Util.Data


init : String -> String -> Id
init network =
    pair (String.toLower network)


initClusterId : String -> Int -> Id
initClusterId network =
    toSafeHex
        >> init network


{-| Hex encoding that is safe for the whole exact-integer range of an Elm Int
(up to 2^53 - 1). rtfeldman/elm-hex's `Hex.toString` recurses on `//`, which
compiles to 32-bit-signed `(n / 16) | 0` — for inputs >= 2^35 the quotient
wraps and the recursion cycles FOREVER, hard-freezing the tab (observed
2026-09-01 when a server minted 48-bit cluster ids). Output is identical to
`Hex.toString` for every value that function could handle, so existing id
strings keep their format.
-}
toSafeHex : Int -> String
toSafeHex n =
    if n < 0 then
        "-" ++ toSafeHexHelp (abs n) ""

    else
        toSafeHexHelp n ""


toSafeHexHelp : Int -> String -> String
toSafeHexHelp n acc =
    let
        digit d =
            Char.fromCode
                (if d < 10 then
                    48 + d

                 else
                    87 + d
                )
    in
    if n < 16 then
        String.cons (digit n) acc

    else
        -- float division by 16 is exact and floor keeps full 53-bit precision,
        -- unlike `//` which truncates the quotient to 32-bit signed
        toSafeHexHelp (floor (toFloat n / 16)) (String.cons (digit (modBy 16 n)) acc)


initFromRecord : { t | address : String, currency : String } -> Id
initFromRecord { address, currency } =
    init currency address


{-| The canonical cluster Id for an address. Fresh-aware: prefers
`freshClusterId` over the legacy `cluster` field (via
`Util.Data.addressCluster`). Every cluster Id derived from an
`Api.Data.Address` must go through this function so cluster-membership
comparisons never mix the fresh and the legacy id space.
-}
initClusterIdFromAddress : Api.Data.Address -> Id
initClusterIdFromAddress address =
    initClusterId address.currency (Util.Data.addressCluster address)


{-| Only for entity records (e.g. `Api.Data.Cluster`), which carry no
`freshClusterId`. Do NOT use with `Api.Data.Address` — use
`initClusterIdFromAddress` instead, which is fresh-aware.
-}
initClusterIdFromRecord : { t | cluster : Int, currency : String } -> Id
initClusterIdFromRecord { cluster, currency } =
    initClusterId currency cluster
