module View.Graph.Layer exposing (..)

import Config.Graph as Graph
import Config.View as View
import Dict
import Log
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id
import Model.Graph.Layer exposing (..)
import Msg.Graph exposing (Msg(..))
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import View.Graph.Entity as Entity
import View.Graph.Link as Link


addresses : View.Config -> Graph.Config -> Id.AddressId -> Layer -> Svg Msg
addresses vc gc selected layer =
    let
        _ =
            Log.log "Graph.addresses" ""
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( Id.entityIdToString entity.id
                , Svg.lazy4 Entity.addresses vc gc selected entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entities : View.Config -> Graph.Config -> Id.EntityId -> Layer -> Svg Msg
entities vc gc selected layer =
    let
        _ =
            Log.log "Graph.entities" ""
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                ( Id.entityIdToString entity.id
                , Svg.lazy4 Entity.entity vc gc selected entity
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entityLinks : View.Config -> Graph.Config -> Id.LinkId Id.EntityId -> Layer -> Svg Msg
entityLinks vc gc hoveredLink layer =
    let
        ( mn, mx ) =
            calcRange vc gc layer

        _ =
            Log.log "Graph.entityLinks" ""
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                Svg.lazy6 Entity.links vc gc mn mx hoveredLink entity
                    :: svg
            )
            []
        |> g []


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
