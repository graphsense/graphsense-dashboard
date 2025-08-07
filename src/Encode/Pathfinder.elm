module Encode.Pathfinder exposing (encode)

import Animation
import Color exposing (Color)
import Dict
import Json.Encode exposing (Value, bool, float, list, null, string)
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)
import Set
import Util.Annotations exposing (AnnotationItem, toList)


encode : Model -> Value
encode model =
    [ string "pathfinder"
    , string "1"
    , string model.name
    , model.network.addresses
        |> Dict.values
        |> list encodeAddress
    , model.network.txs
        |> Dict.values
        |> list encodeTx
    , model.annotations
        |> toList
        |> list encodeAnnotation
    , model.network.aggEdges
        |> Dict.values
        |> list encodeAggEdge
    ]
        |> list identity


encodeAggEdge : AggEdge -> Value
encodeAggEdge edge =
    [ encodeId edge.a
    , encodeId edge.b
    , edge.txs
        |> Set.toList
        |> list encodeId
    ]
        |> list identity


encodeId : Id -> Value
encodeId id =
    [ Id.network id
    , Id.id id
    ]
        |> list string


encodeAddress : Address -> Value
encodeAddress address =
    [ encodeId address.id
    , float address.x
    , float (Animation.getTo address.y)
    , bool address.isStartingPoint
    ]
        |> list identity


encodeTx : Tx -> Value
encodeTx tx =
    [ encodeId tx.id
    , float tx.x
    , float (Animation.getTo tx.y)
    , bool tx.isStartingPoint
    ]
        |> list identity


encodeAnnotation : ( Id, AnnotationItem ) -> Value
encodeAnnotation ( id, annotation ) =
    [ id |> encodeId
    , annotation.label |> string
    , annotation.color |> encodeColor
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
