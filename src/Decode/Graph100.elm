module Decode.Graph100 exposing (decoder)

import Dict exposing (Dict)
import Init.Graph.Id as Id
import Json.Decode exposing (..)
import Model.Graph exposing (..)
import Model.Graph.Id as Id
import Model.Graph.Tag as Tag


decoder : Decoder Deserialized
decoder =
    map2 Deserialized
        (index 1 decodeAddresses)
        (index 2 decodeEntities)


decodeAddresses : Decoder (List DeserializedAddress)
decodeAddresses =
    index 0 decodeAddressId
        |> andThen
            (\addressId ->
                map4 DeserializedAddress
                    (succeed addressId)
                    (index 1 float)
                    (index 2 float)
                    (index 3 (decodeUserTag addressId |> maybe))
            )
        |> list


decodeEntities : Decoder (List DeserializedEntity)
decodeEntities =
    map4 DeserializedEntity
        (index 0 decodeEntityId)
        (index 1 (string |> map Just))
        (index 2 float)
        (index 3 float)
        |> list


decodeAddressId : Decoder Id.AddressId
decodeAddressId =
    map3
        (\currency layer address ->
            Id.initAddressId
                { currency = currency
                , id = address
                , layer = layer
                }
        )
        (index 0 string)
        (index 1 int)
        (index 2 string)


decodeEntityId : Decoder Id.EntityId
decodeEntityId =
    map3
        (\currency layer entity ->
            Id.initEntityId
                { currency = currency
                , id = entity
                , layer = layer
                }
        )
        (index 0 string)
        (index 1 int)
        (index 2 int)


decodeUserTag : Id.AddressId -> Decoder Tag.UserTag
decodeUserTag id =
    map4
        (\label source category abuse ->
            { label = label
            , source = source
            , category = category
            , abuse = abuse
            , currency = Id.currency id
            , address = Id.addressId id
            }
        )
        (index 0 string)
        (index 1 string)
        (index 2 (maybe string))
        (index 3 (maybe string))
