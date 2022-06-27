module Model.Address exposing (..)

import Json.Decode
import Json.Encode
import Model.Graph.Id as Id exposing (AddressId)


type alias Address =
    { currency : String
    , address : String
    }


type alias Addresslink =
    { currency : String
    , source : String
    , target : String
    }


fromId : AddressId -> Address
fromId id =
    { currency = Id.currency id
    , address = Id.addressId id
    }


decoder : Json.Decode.Decoder Address
decoder =
    Json.Decode.map2 Address
        (Json.Decode.field "currency" Json.Decode.string)
        (Json.Decode.field "address" Json.Decode.string)


encoder : Address -> Json.Encode.Value
encoder address =
    Json.Encode.object
        [ ( "currency", Json.Encode.string address.currency )
        , ( "address", Json.Encode.string address.address )
        ]
