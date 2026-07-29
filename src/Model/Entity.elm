module Model.Entity exposing (Entity, isPossibleServiceUtxo)

import Api.Data


type alias Entity =
    { currency : String
    , entity : Int
    }


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
