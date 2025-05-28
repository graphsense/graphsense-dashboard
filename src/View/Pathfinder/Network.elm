module View.Pathfinder.Network exposing (addresses, edges, txs)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import RemoteData exposing (WebData)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Annotations as Annotations
import View.Pathfinder.Address as Address
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


txs : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Dict Id Tx -> Svg Msg
txs plugins vc gc annotations =
    Dict.foldl
        (\id tx svg ->
            ( Id.toString id
            , Annotations.getAnnotation id annotations
                |> Svg.lazy5 Tx.view plugins vc gc tx
            )
                :: svg
        )
        []
        >> Keyed.node "g" []


edges : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> Dict Id Tx -> Svg Msg
edges plugins vc gc addrs =
    Dict.foldl
        (\_ tx svg ->
            Tx.edge plugins vc gc addrs tx :: svg
        )
        []
        >> Keyed.node "g" []
