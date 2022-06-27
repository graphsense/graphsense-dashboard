module Decode.Graph050 exposing (decoder)

import Color
import Color.Convert
import Dict exposing (Dict)
import Init.Graph.Id as Id
import Json.Decode exposing (..)
import Model.Graph exposing (..)
import Model.Graph.Id as Id
import Model.Graph.Tag as Tag
import Tuple exposing (..)


decoder : Decoder Deserialized
decoder =
    map4 merge
        (index 1 decoderTags)
        (index 2 decoderAddresses)
        (index 2 decoderEntities)
        (index 3 (index 2 decodeHighlights))


type alias Address =
    { id : Id.AddressId
    , x : Float
    , y : Float
    , color : Maybe Color.Color
    }


type alias Entity =
    { id : Id.EntityId
    , x : Float
    , y : Float
    }


type alias CurrenyAddress =
    ( String, String )


merge : Dict CurrenyAddress Tag.UserTag -> List Address -> List Entity -> List ( String, Color.Color ) -> Deserialized
merge tags addresses entities highlights =
    { addresses =
        addresses
            |> List.map
                (\address ->
                    { id = address.id
                    , x = address.x
                    , y = address.y
                    , userTag = Dict.get ( Id.currency address.id, Id.addressId address.id ) tags
                    , color = address.color
                    }
                )
    , entities =
        List.map
            (\e ->
                { id = e.id
                , x = e.x
                , y = e.y
                , rootAddress = Nothing
                , color = Nothing
                }
            )
            entities
    , highlights = highlights
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
                        (field "address" string)
                        (field "label" string)
                        (maybe (field "source" string) |> map (Maybe.withDefault ""))
                        (maybe (field "category" string))
                        (maybe (field "abuse" string))

                else
                    fail "no userdefined"
            )


decoderAddresses : Decoder (List Address)
decoderAddresses =
    decodeAddress
        |> list
        |> index 5


decoderEntities : Decoder (List Entity)
decoderEntities =
    maybe decodeEntityWithoutAddresses
        |> list
        |> map (List.filterMap identity)
        |> index 4


decodeAddress : Decoder Address
decodeAddress =
    map3 (\id ( x, y ) color -> Address id x y color)
        (index 0 decodeAddressId)
        decodeCoords
        (maybe (index 1 (index 4 decodeColor)))


decodeEntityWithoutAddresses : Decoder Entity
decodeEntityWithoutAddresses =
    list (succeed ())
        |> index 4
        |> andThen
            (\addresses ->
                if List.isEmpty addresses then
                    fail "addresses found"

                else
                    map2 (\id ( x, y ) -> Entity id x y)
                        (index 0 decodeEntityId)
                        decodeCoords
            )


decodeCoords : Decoder ( Float, Float )
decodeCoords =
    index 1
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


decodeEntityId : Decoder Id.EntityId
decodeEntityId =
    map3
        (\address layer currency ->
            Id.initEntityId
                { currency = currency
                , id = address
                , layer = layer
                }
        )
        (index 0 int)
        (index 1 int)
        (index 2 string)


decodeColor : Decoder Color.Color
decodeColor =
    andThen
        (Color.Convert.hexToColor
            >> Result.map succeed
            >> Result.withDefault (fail "could not convert color")
        )
        string


decodeHighlights : Decoder (List ( String, Color.Color ))
decodeHighlights =
    map2 pair
        (index 1 string)
        (index 0 decodeColor)
        |> list
