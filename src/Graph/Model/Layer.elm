module Graph.Model.Layer exposing (..)

import Graph.Model.Address exposing (..)
import Graph.Model.Entity exposing (..)


type alias Layer =
    { entities : List Entity
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
