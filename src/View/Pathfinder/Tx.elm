module View.Pathfinder.Tx exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Lazy as Svg
import View.Pathfinder.Tx.AccountTx as AccountTx
import View.Pathfinder.Tx.Utxo as Utxo


view : Plugins -> View.Config -> Pathfinder.Config -> Tx -> Svg Msg
view plugins vc gc tx =
    case tx.type_ of
        Utxo t ->
            Utxo.view plugins vc gc tx t

        Account t ->
            AccountTx.view plugins vc gc tx t


edge : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> Tx -> ( String, Svg Msg )
edge plugins vc gc addresses tx =
    ( Id.toString tx.id
    , case tx.type_ of
        Utxo t ->
            Svg.lazy6 Utxo.edge plugins vc gc (tx.selected || tx.hovered) t tx

        Account t ->
            Svg.lazy7 AccountTx.edge plugins vc gc (tx.selected || tx.hovered) addresses t tx
    )
