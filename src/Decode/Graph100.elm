module Decode.Graph100 exposing (decoder)

import Color
import Dict exposing (Dict)
import Init.Graph.Id as Id
import Json.Decode exposing (..)
import Model.Address as A
import Model.Graph exposing (..)
import Model.Graph.Id as Id
import Model.Graph.Tag as Tag
import Tuple exposing (..)


decoder : Decoder Deserialized
decoder =
    map3 Deserialized
        (index 1 decodeAddresses)
        (index 2 decodeEntities)
        (index 3 decodeHighlights)


decodeAddresses : Decoder (List DeserializedAddress)
decodeAddresses =
    index 0 decodeAddressId
        |> andThen
            (\addressId ->
                map5 DeserializedAddress
                    (succeed addressId)
                    (index 1 float)
                    (index 2 float)
                    (index 3 (decodeUserTag False { currency = Id.currency addressId, address = Id.addressId addressId } |> maybe))
                    (maybe (index 4 decodeColor))
            )
        |> list


decodeEntities : Decoder (List DeserializedEntity)
decodeEntities =
    index 0 decodeEntityId
        |> andThen
            (\entityId ->
                index 1 string
                    |> andThen
                        (\rootAddress ->
                            map6 DeserializedEntity
                                (succeed entityId)
                                (succeed (Just rootAddress))
                                (index 2 float)
                                (index 3 float)
                                (maybe (index 4 decodeColor))
                                (maybe
                                    (index 5
                                        (decodeUserTag True
                                            { currency = Id.currency entityId, address = rootAddress }
                                            |> map TagUserTag
                                        )
                                    )
                                )
                        )
            )
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


decodeUserTag : Bool -> A.Address -> Decoder Tag.UserTag
decodeUserTag isClusterDefiner { currency, address } =
    map4
        (\label source category abuse ->
            { label = label
            , source = source
            , category = category
            , abuse = abuse
            , currency = currency
            , address = address
            , isClusterDefiner = isClusterDefiner
            }
        )
        (index 0 string)
        (index 1 string)
        (index 2 (maybe string))
        (index 3 (maybe string))


decodeColor : Decoder Color.Color
decodeColor =
    map4 (\r g b a -> Color.fromRgba { red = r, green = g, blue = b, alpha = a })
        (index 0 float)
        (index 1 float)
        (index 2 float)
        (index 3 float)


decodeHighlights : Decoder (List ( String, Color.Color ))
decodeHighlights =
    map2 pair
        (index 0 string)
        (index 1 decodeColor)
        |> list
