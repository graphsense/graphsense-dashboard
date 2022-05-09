module View.Graph.Entity exposing (entity)

import Color
import Config.Graph as Graph exposing (AddressLabelType(..), addressesCountHeight, expandHandleWidth, labelHeight)
import Config.View exposing (Config)
import Css exposing (fill)
import Css.Graph as Css
import Dict
import Json.Decode as Dec
import Model.Graph exposing (NodeType(..))
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Msg.Graph exposing (Msg(..))
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Events exposing (..)
import Util.View as Util
import View.Graph.Label as Label
import View.Graph.Node as Node
import View.Graph.Util exposing (rotate, translate)
import View.Locale as Locale


entity : Config -> Graph.Config -> Entity -> Svg Msg
entity vc gc ent =
    let
        color =
            ent.category
                |> Maybe.andThen
                    (\category -> Dict.get category gc.colors)
                |> Maybe.withDefault vc.theme.graph.defaultColor
                |> Color.toHsla
                |> (\hsl ->
                        { hsl
                            | lightness = hsl.lightness * vc.theme.graph.lightnessFactor.entity
                            , saturation = hsl.saturation * vc.theme.graph.saturationFactor.entity
                        }
                   )
                |> Color.fromHsla
                |> Util.toCssColor
    in
    g
        [ Css.entityRoot vc |> css
        , UserClickedEntity ent.id
            |> onClick
        , UserRightClickedEntity ent.id
            |> Dec.succeed
            |> on "contextmenu"
        , UserHoversEntity ent.id
            |> onMouseOver
        , UserLeavesEntity ent.id
            |> onMouseOut
        , translate ent.x ent.y |> transform
        ]
        [ rect
            [ width <| String.fromFloat <| Entity.getWidth ent
            , height <| String.fromFloat <| Entity.getHeight ent
            , Css.entityRect vc
                ++ [ Css.fill color ]
                |> css
            ]
            []
        , Svg.path
            [ Css.entityFrame vc |> css
            , String.Interpolate.interpolate
                "M 0 0 H {0} Z M 0 {1} H {0} Z"
                [ Entity.getWidth ent |> String.fromFloat
                , Entity.getHeight ent |> String.fromFloat
                ]
                |> d
            ]
            []
        , Svg.path
            [ Css.nodeSeparatorToExpandHandle vc Model.Graph.Entity |> css
            , String.Interpolate.interpolate
                "M 0 0 V {0} Z M {1} 0 V {0} Z"
                [ Entity.getHeight ent |> String.fromFloat
                , Entity.getWidth ent |> String.fromFloat
                ]
                |> d
            ]
            []
        , label vc gc ent
        , flags vc gc ent
        , currency vc gc ent
        , addresses vc gc ent
        , Node.expand vc
            gc
            { isOutgoing = False
            , nodeType = Model.Graph.Entity
            , degree = ent.entity.inDegree
            , onClick = UserClickedEntityExpandHandle ent.id
            , width = Entity.getWidth ent
            , height = Entity.getHeight ent
            , color = color
            }
        , Node.expand vc
            gc
            { isOutgoing = True
            , nodeType = Model.Graph.Entity
            , degree = ent.entity.outDegree
            , onClick = UserClickedEntityExpandHandle ent.id
            , width = Entity.getWidth ent
            , height = Entity.getHeight ent
            , color = color
            }
        ]


label : Config -> Graph.Config -> Entity -> Svg Msg
label vc gc ent =
    g
        [ Css.entityLabel vc |> css
        , Graph.padding
            / 2
            + labelHeight
            |> translate Graph.padding
            |> transform
        ]
        [ getLabel vc gc ent
            |> Label.label vc gc
        ]


getLabel : Config -> Graph.Config -> Entity -> String
getLabel vc gc ent =
    ""


flags : Config -> Graph.Config -> Entity -> Svg Msg
flags vc gc ent =
    g
        [ Css.entityFlags vc |> css
        , Graph.padding
            / 2
            |> translate (Graph.entityWidth - Graph.padding / 2)
            |> transform
        ]
        []


currency : Config -> Graph.Config -> Entity -> Svg Msg
currency vc gc ent =
    g
        [ Css.entityCurrency vc |> css
        , (Graph.padding / 2 + labelHeight / 2)
            |> translate Graph.padding
            |> transform
        ]
        [ text_
            [ textAnchor "start"
            ]
            [ text (String.toUpper ent.entity.currency) ]
        ]


addresses : Config -> Graph.Config -> Entity -> Svg Msg
addresses vc gc ent =
    let
        size =
            List.length ent.addresses
                |> Locale.int vc.locale

        total =
            Locale.int vc.locale ent.entity.noAddresses

        key =
            "{0}/{1} address"
                ++ (if ent.entity.noAddresses > 1 then
                        "es"

                    else
                        ""
                   )

        string =
            Locale.interpolated vc.locale key [ size, total ]
    in
    g
        [ Css.entityAddressesCount vc
            |> css
        , Entity.getHeight ent
            - Graph.padding
            |> translate (Entity.getWidth ent / 2)
            |> transform
        ]
        [ text_
            [ textAnchor "middle"
            ]
            [ text string
            ]
        ]
