module Encode.Pathfinder exposing (..)

import Animation
import Dict
import Json.Encode exposing (..)
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx exposing (Tx)


encode : Model -> Value
encode model =
    [ string "pathfinder"
    , string "1"
    , model.network.addresses
        |> Dict.values
        |> list encodeAddress
    , model.network.txs
        |> Dict.values
        |> list encodeTx
    ]
        |> list identity


encodeId : Id.Id -> Value
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
