module Decode.Graph.Graph050 exposing (decoder)

import Dict exposing (Dict)
import Init.Graph.Id as Id
import Json.Decode exposing (..)
import Model.Graph exposing (..)
import Model.Graph.Id as Id
import Model.Graph.Tag as Tag


decoder : Decoder Deserialized
decoder =
    map2 merge
        (index 1 decoderTags)
        (index 2 decoderGraph)


type alias Address =
    { id : Id.AddressId
    , x : Float
    , y : Float
    }


merge : Dict ( String, String ) Tag.UserTag -> List Address -> Deserialized
merge tags addresses =
    { addresses =
        addresses
            |> List.map
                (\address ->
                    { id = address.id
                    , x = address.x
                    , y = address.y
                    , userTag = Dict.get ( Id.currency address.id, Id.addressId address.id ) tags
                    }
                )
    }


decoderTags : Decoder (Dict ( String, String ) Tag.UserTag)
decoderTags =
    decoderAddressTag
        |> list
        |> map (List.filterMap identity)
        |> map (List.map (\tag -> ( ( tag.currency, tag.address ), tag )))
        |> map Dict.fromList
        |> index 0


decoderAddressTag : Decoder (Maybe Tag.UserTag)
decoderAddressTag =
    decoderFirstTag
        |> field "tags"


decoderFirstTag : Decoder (Maybe Tag.UserTag)
decoderFirstTag =
    maybe decodeUserDefinedTag
        |> list
        |> map (List.filterMap identity >> List.head)


decodeUserDefinedTag : Decoder Tag.UserTag
decodeUserDefinedTag =
    field "isUserDefined" bool
        |> andThen
            (\isUserDefined ->
                if isUserDefined then
                    map6 Tag.UserTag
                        (field "keyspace" string |> map String.toLower)
                        (field "id" string)
                        (field "label" string)
                        (maybe (field "source" string) |> map (Maybe.withDefault ""))
                        (maybe (field "category" string))
                        (maybe (field "abuse" string))

                else
                    fail "no userdefined"
            )


decoderGraph : Decoder (List Address)
decoderGraph =
    decodeAddress
        |> list
        |> index 5


decodeAddress : Decoder Address
decodeAddress =
    map2 (\id ( x, y ) -> Address id x y)
        (index 0 decodeAddressId)
        (index 1
            (map4
                (\x y dx dy ->
                    ( x + dx
                    , y + dy
                    )
                )
                (index 0 float)
                (index 1 float)
                (index 2 float)
                (index 3 float)
            )
        )


decodeAddressId : Decoder Id.AddressId
decodeAddressId =
    map3
        (\address layer currency ->
            Id.initAddressId
                { currency = currency
                , id = address
                , layer = layer
                }
        )
        (index 0 string)
        (index 1 int)
        (index 2 string)
