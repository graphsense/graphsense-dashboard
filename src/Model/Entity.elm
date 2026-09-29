module Model.Entity exposing (Entity, isPossibleServiceUtxo)

import Api.Data


type alias Entity =
    { currency : String
    , entity : Int
    }


{-| Client-side fallback only — REMOVABLE together with the local heuristics
in `Model.Pathfinder.Address.getAddressType` once every deployed backend
serves `is_possible_service` on address detail (see the note there).
-}
isPossibleServiceUtxo : Api.Data.Cluster -> Bool
isPossibleServiceUtxo =
    isPossibleService


isPossibleService : Api.Data.Cluster -> Bool
isPossibleService cluster =
    let
        maxClusterSizeUser =
            100

        maxDegreeUser =
            7500
    in
    cluster.noAddresses > maxClusterSizeUser || cluster.inDegree > maxDegreeUser || cluster.outDegree > maxDegreeUser
