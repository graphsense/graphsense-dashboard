module Update.Pathfinder.Relation exposing (deleteTx, insertAggEdge, insertTx, updateAggEdge, updateTx)

import Basics.Extra exposing (flip)
import Dict
import Init.Pathfinder.Relation exposing (initRelation)
import IntDict
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (..)
import Model.Pathfinder.Relation exposing (RelationType(..), Relations, getRelationForAggEdge, getRelationForTx)
import Model.Pathfinder.Tx exposing (Tx)
import RecordSetter exposing (..)


insertTx : Tx -> Relations -> Relations
insertTx tx relations =
    getRelationForTx tx relations
        |> Maybe.map
            (\relation ->
                case relation.type_ of
                    Agg _ ->
                        relations

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
                , relationsMap = 
                    listSeparatedAddressesForTx tx
                    |> (\(inputs, outputs) ->
                    )
                    |> Dict.insert tx.id relations.nextInt relations.txRelationMap
                , nextInt = relations.nextInt + 1
            }


updateTx : Id -> (Tx -> Tx) -> Relations -> Relations
updateTx id upd relations =
    getRelationForTx id relations
        |> Maybe.map
            (\relation ->
                case relation.type_ of
                    Agg _ ->
                        relations

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
                    Agg _ ->
                        relations

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


insertAggEdge : AggEdge -> Relations -> Relations
insertAggEdge aggEdge relations =
    getRelationForAggEdge aggEdge.id relations
        |> Maybe.map
            (\relation ->
                case relation.type_ of
                    Agg _ ->
                        Agg aggEdge
                            |> flip s_type_ relation
                            |> flip (IntDict.insert relation.id) relations.relations
                            |> flip s_relations relations

                    Txs _ ->
                        relations
            )
        |> Maybe.withDefault
            { relations
                | relations =
                    aggEdge
                        |> Agg
                        |> initRelation relations.nextInt
                        |> flip (IntDict.insert relations.nextInt) relations.relations
                , aggEdgeRelationMap = Dict.insert aggEdge.id relations.nextInt relations.aggEdgeRelationMap
                , nextInt = relations.nextInt + 1
            }


updateAggEdge : ( Id, Id ) -> (AggEdge -> AggEdge) -> Relations -> Relations
updateAggEdge id upd relations =
    getRelationForAggEdge id relations
        |> Maybe.map
            (\relation ->
                case relation.type_ of
                    Agg agg ->
                        upd agg
                            |> Agg
                            |> flip s_type_ relation
                            |> flip (IntDict.insert relation.id) relations.relations
                            |> flip s_relations relations

                    Txs _ ->
                        relations
            )
        |> Maybe.withDefault relations
