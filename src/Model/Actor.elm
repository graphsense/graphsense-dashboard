module Model.Actor exposing (Actor, decoder, encoder)

import Json.Decode
import Json.Encode


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
