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
import Tuple exposing (first, mapSecond, second)
import Util.Annotations as Annotations
import View.Pathfinder.Address as Address
import View.Pathfinder.AggEdge as AggEdge exposing (edge)
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
                        rel :: agg

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
                txs_
                    |> List.map
                        (\tx ->
                            ( Id.toString tx.id
                            , Svg.g
                                []
                                [ Annotations.getAnnotation tx.id annotations
                                    |> Svg.lazy5 Tx.view plugins vc gc tx
                                , Tx.edge plugins vc gc tx
                                ]
                            )
                        )
                    |> (++)
                        (agg
                            |> List.filterMap
                                (\edge ->
                                    Maybe.map2 (aggEdge plugins vc gc edge)
                                        edge.fromAddress
                                        edge.toAddress
                                )
                        )
           )
        >> Keyed.node "g" []


aggEdge : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdge _ vc _ edge fromAddress toAddress =
    ( Id.toString (first edge.id) ++ Id.toString (second edge.id)
    , Svg.g
        []
        [ Svg.lazy4 AggEdge.view vc edge fromAddress toAddress
        ]
    )



{-
   txs : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> List Tx -> Svg Msg
   txs plugins vc gc annotations =
       List.foldl
           (\tx svg ->
               ( Id.toString tx.id
               , Annotations.getAnnotation tx.id annotations
                   |> Svg.lazy5 Tx.view plugins vc gc tx
               )
                   :: svg
           )
           []
           >> Keyed.node "g" []
-}
