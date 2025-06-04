module Update.Pathfinder.Relation exposing (deleteTx, insertTx, updateTx)

import Basics.Extra exposing (flip)
import Dict
import Init.Pathfinder.Relation exposing (initRelation)
import IntDict
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Relation exposing (RelationType(..), Relations, getRelationForTx)
import Model.Pathfinder.Tx exposing (Tx)
import RecordSetter exposing (..)


insertTx : Tx -> Relations -> Relations
insertTx tx relations =
    getRelationForTx tx.id relations
        |> Maybe.map
            (\relation ->
                case relation.type_ of
                    Txs txs ->
                        Dict.insert tx.id tx txs
                            |> Txs
                            |> flip s_type_ relation
                            |> flip (IntDict.insert relation.id) relations.relations
                            |> flip s_relations relations
            )
        |> Maybe.withDefault
            { relations
                | relations =
                    Dict.singleton tx.id tx
                        |> Txs
                        |> initRelation relations.nextInt
                        |> flip (IntDict.insert relations.nextInt) relations.relations
                , txRelationMap = Dict.insert tx.id relations.nextInt relations.txRelationMap
                , nextInt = relations.nextInt + 1
            }


updateTx : Id -> (Tx -> Tx) -> Relations -> Relations
updateTx id upd relations =
    getRelationForTx id relations
        |> Maybe.map
            (\relation ->
                case relation.type_ of
                    Txs txs ->
                        Dict.update id (Maybe.map upd) txs
                            |> Txs
                            |> flip s_type_ relation
                            |> flip (IntDict.insert relation.id) relations.relations
                            |> flip s_relations relations
            )
        |> Maybe.withDefault relations


deleteTx : Id -> Relations -> Relations
deleteTx id relations =
    getRelationForTx id relations
        |> Maybe.map
            (\relation ->
                case relation.type_ of
                    Txs txs ->
                        let
                            newDict =
                                Dict.remove id txs

                            newRelations =
                                relations.txRelationMap
                                    |> Dict.remove id
                                    |> flip s_txRelationMap relations
                        in
                        if Dict.isEmpty newDict then
                            IntDict.remove relation.id relations.relations
                                |> flip s_relations newRelations

                        else
                            newDict
                                |> Txs
                                |> flip s_type_ relation
                                |> flip (IntDict.insert relation.id) relations.relations
                                |> flip s_relations newRelations
            )
        |> Maybe.withDefault relations
