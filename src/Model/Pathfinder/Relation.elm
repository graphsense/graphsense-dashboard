module Model.Pathfinder.Relation exposing (Relation, RelationType(..), Relations, getRelationForTx)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)


type alias Relations =
    { relations : IntDict Relation
    , txRelationMap : Dict Id Int
    , nextInt : Int
    }


type alias Relation =
    { id : Int
    , type_ : RelationType
    }


type RelationType
    = Txs (Dict Id Tx)


getRelationForTx : Id -> Relations -> Maybe Relation
getRelationForTx id relations =
    Dict.get id relations.txRelationMap
        |> Maybe.andThen (flip IntDict.get relations.relations)



{-
   type alias Relation =
       { from : Id
       , to : Id
       , fromAddress : Maybe Address
       , toAddress : Maybe Address
       , data : Api.Data.NeighborAddress
       , hovered : Bool
       , selected : Bool
       , clock : Clock
       , opacity : Animation
       }
-}
