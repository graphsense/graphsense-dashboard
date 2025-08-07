module View.Pathfinder.Tx exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import Svg.Styled exposing (Svg)
import Svg.Styled.Lazy as Svg
import Util.Annotations as Annotations
import View.Pathfinder.Tx.AccountTx as AccountTx
import View.Pathfinder.Tx.Utxo as Utxo


view : Plugins -> View.Config -> Pathfinder.Config -> Tx -> Maybe Annotations.AnnotationItem -> Svg Msg
view plugins vc gc tx annotation =
    case tx.type_ of
        Utxo t ->
            annotation
                |> Utxo.view plugins vc gc tx t

        Account t ->
            annotation
                |> AccountTx.view plugins vc gc tx t


edge : Plugins -> View.Config -> Pathfinder.Config -> Tx -> Maybe Annotations.AnnotationItem -> Svg Msg
edge plugins vc gc tx annotation =
    case tx.type_ of
        Utxo t ->
            Svg.lazy7 Utxo.edge plugins vc gc (tx.selected || tx.hovered) t tx annotation

        Account t ->
            Svg.lazy7 AccountTx.edge plugins vc gc (tx.selected || tx.hovered) t tx annotation
