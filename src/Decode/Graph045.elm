module Decode.Graph045 exposing (decoder)

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
    map5 merge
        (index 1 decoderTags)
        (index 1 decoderEntityTags)
        (index 2 decoderAddresses)
        (index 2 decoderEntities)
        (succeed [])


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
    , color : Maybe Color.Color
    , noAddresses : Int
    }


type alias CurrencyAddress =
    ( String, String )


type alias CurrencyEntity =
    ( String, Int )


merge : Dict CurrencyAddress Tag.UserTag -> Dict CurrencyEntity DeserializedEntityUserTag -> List Address -> List Entity -> List ( String, Color.Color ) -> Deserialized
merge tags entityTags addresses entities highlights =
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
                , color = e.color
                , userTag =
                    Dict.get ( Id.currency e.id, Id.entityId e.id ) entityTags
                        |> Maybe.map DeserializedEntityUserTagTag
                , noAddresses = e.noAddresses
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


decoderEntityTags : Decoder (Dict ( String, Int ) DeserializedEntityUserTag)
decoderEntityTags =
    decoderEntityTag
        |> list
        |> map (List.filterMap identity)
        |> map (List.map (\tag -> ( ( tag.currency, tag.entity ), tag )))
        |> map Dict.fromList
        |> index 1


decoderAddressTag : Decoder (Maybe Tag.UserTag)
decoderAddressTag =
    decoderFirstTag
        |> field "tags"


decoderEntityTag : Decoder (Maybe DeserializedEntityUserTag)
decoderEntityTag =
    oneOf
        [ int
        , string
            |> andThen
                (\i ->
                    String.toInt i
                        |> Maybe.map succeed
                        |> Maybe.withDefault (fail "no int")
                )
        ]
        |> field "id"
        |> andThen
            (decoderFirstEntityTag
                >> field "tags"
            )


decoderFirstTag : Decoder (Maybe Tag.UserTag)
decoderFirstTag =
    maybe decodeUserDefinedTag
        |> list
        |> map (List.filterMap identity >> List.head)


decoderFirstEntityTag : Int -> Decoder (Maybe DeserializedEntityUserTag)
decoderFirstEntityTag entityId =
    maybe (decodeDeserializedEntityUserDefinedTag entityId)
        |> list
        |> map (List.filterMap identity >> List.head)


decodeUserDefinedTag : Decoder Tag.UserTag
decodeUserDefinedTag =
    field "isUserDefined" bool
        |> andThen
            (\isUserDefined ->
                if isUserDefined then
                    map7 Tag.UserTag
                        (field "keyspace" string |> map String.toLower)
                        (field "address" string)
                        (field "label" string)
                        (maybe (field "source" string) |> map (Maybe.withDefault ""))
                        (maybe (field "category" string))
                        (maybe (field "abuse" string))
                        (succeed True)

                else
                    fail "no userdefined"
            )


decodeDeserializedEntityUserDefinedTag : Int -> Decoder DeserializedEntityUserTag
decodeDeserializedEntityUserDefinedTag entityId =
    field "isUserDefined" bool
        |> andThen
            (\isUserDefined ->
                if isUserDefined then
                    map6 DeserializedEntityUserTag
                        (field "keyspace" string |> map String.toLower)
                        (succeed entityId)
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
    maybe decodeEntity
        |> list
        |> map (List.filterMap identity)
        |> index 4


decodeAddress : Decoder Address
decodeAddress =
    map3 (\id ( x, y ) color -> Address id x y color)
        (index 0 decodeAddressId)
        decodeCoords
        (maybe (index 1 (index 4 decodeColor)))


decodeEntity : Decoder Entity
decodeEntity =
    list (succeed ())
        |> index 4
        |> index 1
        |> andThen
            (\addresses ->
                map3
                    (\id ( x, y ) color ->
                        List.length addresses
                            |> Entity id x y color
                    )
                    (index 0 decodeEntityId)
                    decodeCoords
                    (maybe (index 1 (index 5 decodeColor)))
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
        (index 1 decodeInt)
        (index 2 string)


decodeInt : Decoder Int
decodeInt =
    oneOf
        [ int
        , string
            |> andThen (String.toInt >> Maybe.map succeed >> Maybe.withDefault (fail "no an integer"))
        ]


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
        (index 0 decodeInt)
        (index 1 decodeInt)
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
