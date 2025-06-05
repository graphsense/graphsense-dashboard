module View.Pathfinder.Relation exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Relation exposing (Relation, RelationType(..))
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import Svg.Styled exposing (Svg)
import Svg.Styled.Lazy as Svg
import Util.Annotations as Annotations
import View.Pathfinder.Tx as Tx


view : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Relation -> List ( String, Svg Msg )
view plugins vc gc annotations relation =
    case relation.type_ of
        Txs txs_ ->
            txs plugins vc gc annotations txs_


txs : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Dict Id Tx -> List ( String, Svg Msg )
txs plugins vc gc annotations =
    Dict.values
        >> List.map
            (\tx ->
                [ ( Id.toString tx.id
                  , Annotations.getAnnotation tx.id annotations
                        |> Svg.lazy5 Tx.view plugins vc gc tx
                  )
                , Tx.edge plugins vc gc tx
                ]
            )
        >> List.concat
