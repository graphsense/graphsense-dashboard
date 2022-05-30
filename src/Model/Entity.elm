module Model.Entity exposing (..)

import Json.Decode
import Json.Encode
import Model.Graph.Id as Id exposing (EntityId)


type alias Entity =
    { currency : String
    , entity : Int
    }


fromId : EntityId -> Entity
fromId id =
    { currency = Id.currency id
    , entity = Id.entityId id
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
