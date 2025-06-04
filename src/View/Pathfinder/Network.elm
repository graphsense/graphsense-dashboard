module View.Pathfinder.Network exposing (addresses, relations, txs)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import IntDict
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Relation exposing (Relations)
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
import View.Pathfinder.Relation as Relation
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


relations : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> Relations -> Svg Msg
relations plugins vc gc annotations =
    .relations
        >> IntDict.foldl
            (\_ rel svg ->
                Relation.view plugins vc gc annotations rel
                    ++ svg
            )
            []
        >> Keyed.node "g" []


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
