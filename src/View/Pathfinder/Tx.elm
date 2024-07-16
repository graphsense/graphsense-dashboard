module View.Pathfinder.Tx exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import View.Pathfinder.Tx.AccountTx as AccountTx
import View.Pathfinder.Tx.Utxo as Utxo


view : Plugins -> View.Config -> Pathfinder.Config -> Tx -> Svg Msg
view plugins vc gc tx =
    case tx.type_ of
        Utxo t ->
            Utxo.view plugins vc gc tx.id (tx.selected || tx.hovered) t

        Account _ ->
            text ""


edge : Plugins -> View.Config -> Pathfinder.Config -> Tx -> ( String, Svg Msg )
edge plugins vc gc tx =
    ( Id.toString tx.id
    , case tx.type_ of
        Utxo t ->
            Svg.lazy5 Utxo.edge plugins vc gc (tx.selected || tx.hovered) t

        Account t ->
            AccountTx.edge plugins vc gc Dict.empty t
    )
