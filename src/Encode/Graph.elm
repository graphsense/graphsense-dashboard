module Encode.Graph exposing (..)

import Dict
import Json.Encode exposing (..)
import Model.Graph exposing (..)
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Model.Graph.Tag as Tag


encode : String -> Model -> Value
encode version model =
    [ string version
    , Layer.addresses model.layers
        |> list encodeAddress
    , Layer.entities model.layers
        |> List.filter (.addresses >> Dict.isEmpty)
        |> list encodeEntity
    ]
        |> list identity


encodeAddressId : Id.AddressId -> Value
encodeAddressId id =
    [ Id.currency id |> string
    , Id.layer id |> int
    , Id.addressId id |> string
    ]
        |> list identity


encodeAddress : Address -> Value
encodeAddress address =
    [ encodeAddressId address.id
    , float address.x
    , float address.y
    , encodeUserTag address.userTag
    ]
        |> list identity


encodeEntityId : Id.EntityId -> Value
encodeEntityId id =
    [ Id.currency id |> string
    , Id.layer id |> int
    , Id.entityId id |> int
    ]
        |> list identity


encodeEntity : Entity -> Value
encodeEntity entity =
    [ encodeEntityId entity.id
    , string entity.entity.rootAddress
    , float entity.x
    , float entity.y
    ]
        |> list identity


encodeUserTag : Maybe Tag.UserTag -> Value
encodeUserTag =
    Maybe.map
        (\tag ->
            [ string tag.label
            , string tag.source
            , tag.category
                |> Maybe.map string
                |> Maybe.withDefault null
            , tag.abuse
                |> Maybe.map string
                |> Maybe.withDefault null
            ]
                |> list identity
        )
        >> Maybe.withDefault null
