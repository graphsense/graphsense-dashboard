module View.Pathfinder.Network exposing (addresses, relations)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import RemoteData exposing (WebData)
import Set
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Tuple exposing (mapSecond)
import Util.Annotations as Annotations
import View.Pathfinder.Address as Address
import View.Pathfinder.AggEdge as AggEdge
import View.Pathfinder.Tx as Tx


addresses : Plugins -> View.Config -> Pathfinder.Config -> Colors.ScopedColorAssignment -> Dict Id (WebData Api.Data.Entity) -> Annotations.AnnotationModel -> Dict Id Address -> Svg Msg
addresses plugins vc pc colors clusters annotations =
    Dict.foldl
        (\id address svg ->
            ( Id.toString id
            , Annotations.getAnnotation id annotations
                |> Svg.lazy7 Address.view plugins vc pc colors address (flip Dict.get clusters)
            )
                :: svg
        )
        []
        >> Keyed.node "g" []


relations : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Dict Id Tx -> Dict ( Id, Id ) AggEdge -> Svg Msg
relations plugins vc gc annotations txs =
    Dict.foldl
        (\_ rel ( agg, txs_ ) ->
            case gc.tracingMode of
                Pathfinder.AggregateTracingMode ->
                    ( rel :: agg, txs_ )

                Pathfinder.TransactionTracingMode ->
                    ( if Set.isEmpty rel.txs then
                        if rel.alwaysShow then
                            rel :: agg

                        else
                            agg

                      else
                        agg
                    , Set.union txs_ rel.txs
                    )
        )
        ( [], Set.empty )
        >> mapSecond
            (Set.foldl
                (\txId txs_ ->
                    Dict.get txId txs
                        |> Maybe.map (flip (::) txs_)
                        |> Maybe.withDefault txs_
                )
                []
            )
        >> (\( agg, txs_ ) ->
                [ txs_
                    |> List.map
                        (\tx ->
                            ( Id.toString tx.id |> (++) "te"
                            , Annotations.getAnnotation tx.id annotations
                                |> Tx.edge plugins vc gc tx
                            )
                        )
                    |> Keyed.node "g" []
                , txs_
                    |> List.map
                        (\tx ->
                            ( Id.toString tx.id |> (++) "tn"
                            , Annotations.getAnnotation tx.id annotations
                                |> Svg.lazy5 Tx.view plugins vc gc tx
                            )
                        )
                    |> Keyed.node "g" []
                , agg
                    |> List.filterMap
                        (\edge ->
                            Maybe.map2 (aggEdgeEdge plugins vc gc edge)
                                edge.aAddress
                                edge.bAddress
                        )
                    |> Keyed.node "g" []
                , agg
                    |> List.filterMap
                        (\edge ->
                            Maybe.map2 (aggEdgeNode plugins vc gc edge)
                                edge.aAddress
                                edge.bAddress
                        )
                    |> Keyed.node "g" []
                , agg
                    |> List.filter (\a -> a.selected || a.hovered)
                    |> List.filterMap
                        (\edge ->
                            Maybe.map2 (aggEdgeNodeHighlight plugins vc gc edge)
                                edge.aAddress
                                edge.bAddress
                        )
                    |> Keyed.node "g" []
                ]
                    |> Svg.g []
           )


aggEdgeNodeHighlight : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNodeHighlight _ vc _ edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "eh"
    , Svg.lazy4 AggEdge.highlight vc edge aAddress bAddress
    )


aggEdgeNode : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNode _ vc _ edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "en"
    , Svg.lazy4 AggEdge.view vc edge aAddress bAddress
    )


aggEdgeEdge : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeEdge _ vc _ edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "ee"
    , Svg.lazy5 AggEdge.edge vc edge aAddress bAddress False
    )



{-
   txs : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> List Tx -> Svg Msg
   txs plugins vc gc annotations =
       List.foldl
           (\tx svg ->
               ( Id.bString tx.id
               , Annotations.getAnnotation tx.id annotations
                   |> Svg.lazy5 Tx.view plugins vc gc tx
               )
                   :: svg
           )
           []
           >> Keyed.node "g" []
-}
