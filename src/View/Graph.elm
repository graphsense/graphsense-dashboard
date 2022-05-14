module View.Graph exposing (view)

import Config.Graph as Graph
import Config.View exposing (Config)
import Css.Graph as Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import IntDict exposing (IntDict)
import Json.Decode
import List.Extra
import Model.Graph exposing (..)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Transform as Transform
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import Util.Graph as Util
import View.Graph.Address as Address
import View.Graph.Entity as Entity
import View.Graph.Layer as Layer
import View.Graph.Navbar as Navbar
import View.Graph.Transform as Transform


view : Config -> Model -> Html Msg
view vc model =
    section
        [ Css.root vc |> Html.css
        ]
        [ Navbar.navbar vc
        , graph vc model.config model
        ]


graph : Config -> Graph.Config -> Model -> Html Msg
graph vc gc model =
    Html.section
        [ Css.graphRoot vc |> Html.css
        ]
        [ svg
            [ preserveAspectRatio "xMidYMid slice"
            , Svg.id "graph"
            , Transform.viewBox { width = model.width, height = model.height } model.transform |> viewBox
            , Css.svgRoot vc |> Svg.css
            , Svg.custom "wheel"
                (Json.Decode.map3
                    (\x y z ->
                        { message = UserWheeledOnGraph x y z
                        , stopPropagation = True
                        , preventDefault = False
                        }
                    )
                    (Json.Decode.field "deltaX" Json.Decode.float)
                    (Json.Decode.field "deltaY" Json.Decode.float)
                    (Json.Decode.field "deltaZ" Json.Decode.float)
                )
            , Svg.on "mousedown"
                (Util.decodeCoords Coords
                    |> Json.Decode.map UserPushesLeftMouseButtonOnGraph
                )

            {- , Svg.preventDefaultOn "mousemove"
               (Util.decodeCoords Coords
                   |> Json.Decode.map (\c -> ( UserMovesMouseOnGraph c, True ))
                   )
            -}
            ]
            [ Svg.lazy3 entities vc gc model.layers
            , Svg.lazy3 addresses vc gc model.layers
            , Svg.lazy3 entityLinks vc gc model.layers
            ]
        ]


addresses : Config -> Graph.Config -> IntDict Layer -> Svg Msg
addresses vc gc =
    Layer.addresses
        >> List.map (Address.address vc gc)
        >> g []


entities : Config -> Graph.Config -> IntDict Layer -> Svg Msg
entities vc gc =
    Layer.entities
        >> List.map (Entity.entity vc gc)
        >> g []


entityLinks : Config -> Graph.Config -> IntDict Layer -> Svg Msg
entityLinks vc gc =
    IntDict.foldl
        (\layerId layer svg ->
            Svg.lazy3 Layer.entityLinks vc gc layer
                :: svg
        )
        []
        >> g []
