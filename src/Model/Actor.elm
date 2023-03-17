module Model.Actor exposing (..)

import Json.Decode
import Json.Encode
import Model.Graph.Id as Id exposing (AddressId)


type alias Actor =
    { actorId : String
    }


decoder : Json.Decode.Decoder Actor
decoder =
    Json.Decode.map Actor
        (Json.Decode.field "actorId" Json.Decode.string)


encoder : Actor -> Json.Encode.Value
encoder address =
    Json.Encode.object
        [ ( "actorId", Json.Encode.string address.actorId )
        ]
