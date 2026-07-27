module Model.Entity exposing (Entity, Entitylink, decoder, encoder, isPossibleService, isPossibleServiceUtxo)

import Api.Data
import Json.Decode
import Json.Encode


type alias Entity =
    { currency : String
    , entity : Int
    }


type alias Entitylink =
    { currency : String
    , source : Int
    , target : Int
    }


decoder : Json.Decode.Decoder Entity
decoder =
    Json.Decode.map2 Entity
        (Json.Decode.field "currency" Json.Decode.string)
        (Json.Decode.field "entity" Json.Decode.int)


encoder : Entity -> Json.Encode.Value
encoder entity =
    Json.Encode.object
        [ ( "currency", Json.Encode.string entity.currency )
        , ( "entity", Json.Encode.int entity.entity )
        ]


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
