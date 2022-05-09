module Model.Graph.Layer exposing (..)

import Model.Graph.Address exposing (..)
import Model.Graph.Entity exposing (..)


type alias Layer =
    { id : Int
    , entities : List Entity
    , x : Float
    }


addresses : List Layer -> List Address
addresses =
    List.foldl
        (\layer addrs ->
            List.foldl
                (\entity addrs_ ->
                    addrs_ ++ entity.addresses
                )
                addrs
                layer.entities
        )
        []


entities : List Layer -> List Entity
entities =
    List.foldl
        (\layer ents ->
            ents ++ layer.entities
        )
        []
