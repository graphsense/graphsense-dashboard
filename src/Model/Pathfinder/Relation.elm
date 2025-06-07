module Model.Pathfinder.Relation exposing (Relation, RelationType(..), Relations, getRelationForAggEdge, getRelationForTx)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import List.Extra
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx, listAddressesForTx, listSeparatedAddressesForTx)
import Tuple exposing (first, second)


type alias Relations =
    { relations : IntDict Relation
    , relationsMap : Dict ( Id, Id ) Int
    , nextInt : Int
    }


type alias Relation =
    { id : Int
    , type_ : RelationType
    }


type RelationType
    = Txs (Dict Id Tx)
    | Agg AggEdge


getRelationForTx : Tx -> Relations -> Maybe Relation
getRelationForTx tx relations =
    let
        ( inputs, outputs ) =
            listSeparatedAddressesForTx tx
    in
    Maybe.Extra.andThen2
        (\from to ->
            Dict.get ( from.id, to.id ) relations.relationsMap
                |> Maybe.andThen (flip IntDict.get relations.relations)
        )
        (List.head inputs)
        (List.head outputs)


getRelationForAggEdge : ( Id, Id ) -> Relations -> Maybe Relation
getRelationForAggEdge id relations =
    Dict.get id relations.relationsMap
        |> Maybe.andThen (flip IntDict.get relations.relations)
