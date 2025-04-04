module Encode.Graph exposing (encode)

import Color exposing (Color)
import Json.Encode exposing (..)
import Model.Graph exposing (..)
import Model.Graph.Address exposing (Address)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Model.Graph.Tag as Tag


encode : Model -> Value
encode model =
    [ string "1.0.0"
    , Layer.addresses model.layers
        |> list encodeAddress
    , Layer.entities model.layers
        |> list encodeEntity
    , model.highlights.highlights |> encodeHighlights
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
    , encodeColor address.color
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
    , encodeColor entity.color
    , encodeUserTag entity.userTag
    ]
        |> list identity


encodeColor : Maybe Color -> Value
encodeColor =
    Maybe.map Color.toRgba
        >> Maybe.map
            (\c ->
                [ c.red
                , c.green
                , c.blue
                , c.alpha
                ]
                    |> list float
            )
        >> Maybe.withDefault null


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
            , tag.isClusterDefiner
                |> bool
            ]
                |> list identity
        )
        >> Maybe.withDefault null


encodeHighlights : List ( String, Color ) -> Value
encodeHighlights colors =
    colors
        |> List.map (\( s, c ) -> list identity [ string s, Just c |> encodeColor ])
        |> list identity
