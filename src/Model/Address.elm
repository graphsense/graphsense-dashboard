module Model.Address exposing (Address, Addresslink, decoder, encoder, equals, fromId, fromPathfinderId)

import Json.Decode
import Json.Encode
import Model.Graph.Id as Id exposing (AddressId)
import Model.Pathfinder.Id as Pathfinder


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


fromPathfinderId : Pathfinder.Id -> Address
fromPathfinderId id =
    { currency = Pathfinder.network id
    , address = Pathfinder.id id
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


equals : Address -> Address -> Bool
equals a b =
    let
        fn x =
            x.currency
                ++ x.address
                |> String.toLower
    in
    fn a == fn b
