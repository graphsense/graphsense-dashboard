module View.Pathfinder.Tx exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Tuple exposing (first)
import View.Pathfinder.Tx.AccountTx as AccountTx
import View.Pathfinder.Tx.Utxo as Utxo


view : Plugins -> View.Config -> Pathfinder.Config -> Tx -> Svg Msg
view plugins vc gc tx =
    case tx.type_ of
        Utxo t ->
            Utxo.view plugins vc gc tx.id t

        Account _ ->
            text ""


edge : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> Tx -> ( String, Svg Msg )
edge plugins vc gc addresses tx =
    ( Id.toString tx.id
    , case tx.type_ of
        Utxo t ->
            Utxo.edge plugins vc gc addresses t

        Account t ->
            AccountTx.edge plugins vc gc addresses t
    )