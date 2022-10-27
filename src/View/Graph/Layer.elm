module View.Graph.Layer exposing (..)

import Config.Graph as Graph
import Config.View as View
import Dict
import Log
import Model.Graph.Address as Address
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id
import Model.Graph.Layer exposing (..)
import Msg.Graph exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Tuple exposing (..)
import Tuple2 exposing (uncurry)
import View.Graph.Entity as Entity
import View.Graph.Link as Link


addresses : Plugins -> View.Config -> Graph.Config -> Layer -> Svg Msg
addresses plugins vc gc layer =
    let
        _ =
            Log.log "Layer.addresses" layer.id
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( Id.entityIdToString entity.id
                , Svg.lazy4 Entity.addresses plugins vc gc entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entities : Plugins -> View.Config -> Graph.Config -> Layer -> Svg Msg
entities plugins vc gc layer =
    let
        _ =
            Log.log "Layer.entities" layer.id
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( Id.entityIdToString entity.id
                , Svg.lazy4 Entity.entity plugins vc gc entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entityLinks : View.Config -> Graph.Config -> Layer -> Svg Msg
entityLinks vc gc layer =
    let
        ( mn, mx ) =
            calcRange vc gc layer

        _ =
            Log.log "Layer.entityLinks" layer.id
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( "entityLinks" ++ Id.entityIdToString entity.id
                , Svg.lazy5 Entity.links vc gc mn mx entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entityShadowLinks : View.Config -> Layer -> Svg Msg
entityShadowLinks vc layer =
    let
        _ =
            Log.log "Layer.entityShadowLinks" layer.id
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( "entityShadowLinks" ++ Id.entityIdToString entity.id
                , Svg.lazy2 Entity.shadowLinks vc entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


addressShadowLinks : View.Config -> Layer -> Svg Msg
addressShadowLinks vc layer =
    let
        _ =
            Log.log "Layer.addressShadowLinks" layer.id
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( "addressesShadowLinks" ++ Id.entityIdToString entity.id
                , Svg.lazy2 Entity.addressShadowLinks vc entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


addressLinks : View.Config -> Graph.Config -> Layer -> Svg Msg
addressLinks vc gc layer =
    let
        ( mn, mx ) =
            calcAddressRange vc gc layer

        _ =
            Log.log "Layer.addressLinks" layer.id
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( "addressLinks" ++ Id.entityIdToString entity.id
                , Svg.lazy5 Entity.addressLinks vc gc mn mx entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


calcRange : View.Config -> Graph.Config -> Layer -> ( Float, Float )
calcRange vc gc layer =
    layer.entities
        |> Dict.foldl
            (\_ entity ( mn, mx ) ->
                case entity.links of
                    Entity.Links links ->
                        Dict.foldl
                            (\_ link ( mn_, mx_ ) ->
                                let
                                    a =
                                        Link.getLinkAmount vc gc link
                                in
                                ( Basics.min mn_ a
                                , Basics.max mx_ a
                                )
                            )
                            ( mn, mx )
                            links
            )
            ( 0, 0 )


calcAddressRange : View.Config -> Graph.Config -> Layer -> ( Float, Float )
calcAddressRange vc gc layer =
    layer.entities
        |> Dict.foldl
            (\_ entity minmax ->
                entity.addresses
                    |> Dict.foldl
                        (\_ address minmax_ ->
                            case address.links of
                                Address.Links links ->
                                    links
                                        |> Dict.foldl
                                            (\_ link ( mn, mx ) ->
                                                let
                                                    a =
                                                        Link.getLinkAmount vc gc link
                                                in
                                                ( Basics.min mn a
                                                , Basics.max mx a
                                                )
                                            )
                                            minmax_
                        )
                        minmax
            )
            ( 0, 0 )
