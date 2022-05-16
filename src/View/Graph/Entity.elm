module View.Graph.Entity exposing (addresses, entity, links)

import Color
import Config.Graph as Graph exposing (AddressLabelType(..), addressesCountHeight, expandHandleWidth, labelHeight)
import Config.View exposing (Config)
import Css exposing (fill)
import Css.Graph as Css
import Dict
import Json.Decode
import Model.Graph exposing (NodeType(..))
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Transform as Transform
import Msg.Graph exposing (Msg(..))
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg exposing (..)
import Util.Graph exposing (rotate, translate)
import Util.View as Util
import View.Graph.Address as Address
import View.Graph.Label as Label
import View.Graph.Link as Link
import View.Graph.Node as Node
import View.Locale as Locale


addresses : Config -> Graph.Config -> Maybe Id.AddressId -> Entity -> Svg Msg
addresses vc gc selected ent =
    ent.addresses
        |> Dict.foldl
            (\_ address svg ->
                ( Id.addressIdToString address.id
                , Svg.lazy4 Address.address vc gc selected address
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entity : Config -> Graph.Config -> Maybe Id.EntityId -> Entity -> Svg Msg
entity vc gc selected ent =
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

        isSelected =
            selected == Just ent.id
    in
    g
        [ Css.entityRoot vc |> css
        , UserClickedEntity ent.id
            |> onClick
        , UserRightClickedEntity ent.id
            |> Json.Decode.succeed
            |> on "contextmenu"
        , UserHoversEntity ent.id
            |> onMouseOver
        , UserLeavesEntity ent.id
            |> onMouseOut
        , translate (ent.x + ent.dx) (ent.y + ent.dy) |> transform
        , UserPushesLeftMouseButtonOnEntity ent.id
            |> Util.Graph.mousedown
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
            [ isSelected
                |> Css.nodeFrame vc Model.Graph.Entity
                |> css
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
        , addressesCount vc gc ent
        , Node.expand vc
            gc
            { isOutgoing = False
            , nodeType = Model.Graph.Entity
            , degree = ent.entity.inDegree
            , onClick = UserClickedEntityExpandHandle ent.id
            , width = Entity.getWidth ent
            , height = Entity.getHeight ent
            , color = color
            , isSelected = isSelected
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
            , isSelected = isSelected
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
    --ent.entity.entity |> String.fromInt
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


addressesCount : Config -> Graph.Config -> Entity -> Svg Msg
addressesCount vc gc ent =
    let
        size =
            Dict.size ent.addresses
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


links : Config -> Graph.Config -> Float -> Float -> Entity -> Svg Msg
links vc gc mn mx ent =
    case ent.links of
        Entity.Links lnks ->
            lnks
                |> Dict.foldr
                    (\_ link svg ->
                        Svg.lazy6 Link.entityLink vc gc mn mx ent link
                            :: svg
                    )
                    []
                |> g []
