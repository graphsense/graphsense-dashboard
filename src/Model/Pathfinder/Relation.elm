module Model.Pathfinder.Relation exposing (Relation, RelationType(..), Relations, getRelationForAggEdge, getRelationForTx)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)


type alias Relations =
    { relations : IntDict Relation
    , txRelationMap : Dict Id Int
    , aggEdgeRelationMap : Dict ( Id, Id ) Int
    , nextInt : Int
    }


type alias Relation =
    { id : Int
    , type_ : RelationType
    }


type RelationType
    = Txs (Dict Id Tx)
    | Agg AggEdge


getRelationForTx : Id -> Relations -> Maybe Relation
getRelationForTx id relations =
    Dict.get id relations.txRelationMap
        |> Maybe.andThen (flip IntDict.get relations.relations)


getRelationForAggEdge : ( Id, Id ) -> Relations -> Maybe Relation
getRelationForAggEdge id relations =
    Dict.get id relations.aggEdgeRelationMap
        |> Maybe.andThen (flip IntDict.get relations.relations)
