module Init.Pathfinder.Id exposing (init, initClusterId, initClusterIdFromAddress, initClusterIdFromRecord, initFromRecord)

import Api.Data
import Hex
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (pair)
import Util.Data


init : String -> String -> Id
init network =
    pair (String.toLower network)


initClusterId : String -> Int -> Id
initClusterId network =
    Hex.toString
        >> init network


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
