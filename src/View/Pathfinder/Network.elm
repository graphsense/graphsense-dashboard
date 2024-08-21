module View.Pathfinder.Network exposing (addresses, edges, txs)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import View.Pathfinder.Address as Address
import View.Pathfinder.Tx as Tx


addresses : Plugins -> View.Config -> Pathfinder.Config -> Colors.ScopedColorAssignment -> Dict Id Address -> Svg Msg
addresses plugins vc gc colors =
    Dict.foldl
        (\id address svg ->
            ( Id.toString id
            , Svg.lazy5 Address.view plugins vc gc colors address
            )
                :: svg
        )
        []
        >> Keyed.node "g" []


txs : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Tx -> Svg Msg
txs plugins vc gc =
    Dict.foldl
        (\id tx svg ->
            ( Id.toString id
            , Svg.lazy4 Tx.view plugins vc gc tx
            )
                :: svg
        )
        []
        >> Keyed.node "g" []


edges : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Tx -> Svg Msg
edges plugins vc gc =
    Dict.foldl
        (\_ tx svg ->
            Tx.edge plugins vc gc tx :: svg
        )
        []
        >> Keyed.node "g" []
