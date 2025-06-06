module View.Pathfinder.Relation exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Relation exposing (Relation, RelationType(..))
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Lazy as Svg
import Tuple exposing (first, second)
import Util.Annotations as Annotations
import View.Pathfinder.AggEdge as AggEdge
import View.Pathfinder.Tx as Tx


view : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Relation -> List ( String, Svg Msg )
view plugins vc gc annotations relation =
    case relation.type_ of
        Txs txs_ ->
            txs plugins vc gc annotations txs_

        Agg edge ->
            Maybe.map2 (aggEdge plugins vc gc edge)
                edge.fromAddress
                edge.toAddress
                |> Maybe.map List.singleton
                |> Maybe.withDefault []


txs : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Dict Id Tx -> List ( String, Svg Msg )
txs plugins vc gc annotations =
    Dict.values
        >> List.map
            (\tx ->
                [ ( Id.toString tx.id
                  , Svg.g
                        []
                        [ Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy5 Tx.view plugins vc gc tx
                        , Tx.edge plugins vc gc tx
                        ]
                  )
                ]
            )
        >> List.concat


aggEdge : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdge plugins vc gc edge fromAddress toAddress =
    ( Id.toString (first edge.id) ++ Id.toString (second edge.id)
    , Svg.g
        []
        [ Svg.lazy6 AggEdge.view plugins vc gc edge fromAddress toAddress
        ]
    )
