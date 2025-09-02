module View.Pathfinder.Network exposing (addresses, relations)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import RemoteData exposing (WebData)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Tuple3
import Util.Annotations as Annotations
import View.Pathfinder.Address as Address
import View.Pathfinder.AggEdge as AggEdge
import View.Pathfinder.ConversionEdge as ConversionEdge
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


relations : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Dict Id Tx -> Dict ( Id, Id ) AggEdge -> Dict ( Id, Id ) ConversionEdge -> Svg Msg
relations plugins vc gc annotations txs agg conversions =
    (case gc.tracingMode of
        Pathfinder.AggregateTracingMode ->
            ( Dict.values agg, [], [] )

        Pathfinder.TransactionTracingMode ->
            ( []
            , Dict.values txs
            , Dict.values conversions
              --|> List.concat
            )
    )
        |> Tuple3.mapFirst
            (List.filter
                (\edge ->
                    RemoteData.isSuccess edge.a2b && RemoteData.isSuccess edge.b2a
                )
            )
        |> (\( agg_, txs_, conversions_ ) ->
                [ conversions_
                    |> List.filterMap
                        (\conversion ->
                            Maybe.map2 (conversionEdge plugins vc gc conversion)
                                conversion.inputAddress
                                conversion.outputAddress
                        )
                    |> Keyed.node "g" []
                , txs_
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
                , agg_
                    |> List.filterMap
                        (\edge ->
                            Maybe.map2 (aggEdgeEdge plugins vc gc edge)
                                edge.aAddress
                                edge.bAddress
                        )
                    |> Keyed.node "g" []
                , agg_
                    |> List.filterMap
                        (\edge ->
                            Maybe.map2 (aggEdgeNode plugins vc gc edge)
                                edge.aAddress
                                edge.bAddress
                        )
                    |> Keyed.node "g" []
                , agg_
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


conversionEdge : Plugins -> View.Config -> Pathfinder.Config -> ConversionEdge -> Address -> Address -> ( String, Svg Msg )
conversionEdge _ vc _ conversion aAddress bAddress =
    ( ConversionEdge.toIdString conversion |> (++) "ce"
    , Svg.lazy4 ConversionEdge.view vc conversion aAddress bAddress
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
