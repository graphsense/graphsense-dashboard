module View.Graph.Layer exposing (..)

import Config.Graph as Graph
import Config.View as View
import Dict
import Model.Graph.Entity as Entity
import Model.Graph.Layer exposing (..)
import Msg.Graph exposing (Msg(..))
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import View.Graph.Entity as Entity
import View.Graph.Link as Link


entityLinks : View.Config -> Graph.Config -> Layer -> Svg Msg
entityLinks vc gc layer =
    let
        ( mn, mx ) =
            calcRange vc gc layer
    in
    layer.entities
        |> Dict.foldl
            (\_ entity svg ->
                Svg.lazy5 Entity.links vc gc mn mx entity
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
