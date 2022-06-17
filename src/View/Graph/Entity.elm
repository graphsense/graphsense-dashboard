module View.Graph.Entity exposing (addressLinks, addresses, entity, links)

import Color
import Config.Graph as Graph exposing (AddressLabelType(..), addressesCountHeight, expandHandleWidth, labelHeight)
import Config.View exposing (Config)
import Css exposing (fill)
import Css.Graph as Css
import Dict
import Init.Graph.Id as Id
import Json.Decode
import Log
import Maybe.Extra
import Model.Graph exposing (NodeType(..))
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Transform as Transform
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (Plugins)
import Plugin.View.Graph.Entity
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg exposing (..)
import Tuple exposing (..)
import Util.Graph exposing (decodeCoords, rotate, scale, translate)
import Util.View as Util
import View.Graph.Address as Address
import View.Graph.Label as Label
import View.Graph.Link as Link
import View.Graph.Node as Node
import View.Locale as Locale


addresses : Plugins -> Config -> Graph.Config -> String -> Entity -> Svg Msg
addresses plugins vc gc selected ent =
    ent.addresses
        |> Dict.foldl
            (\_ address svg ->
                ( Id.addressIdToString address.id
                , Svg.lazy5 Address.address plugins vc gc selected address
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entity : Plugins -> Config -> Graph.Config -> String -> Entity -> Svg Msg
entity plugins vc gc selected ent =
    let
        _ =
            Log.log "rednerEntity" ent.id

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
            selected == Id.entityIdToString ent.id

        rectX =
            String.fromFloat expandHandleWidth
    in
    g
        [ Css.entityRoot vc |> css
        , Json.Decode.succeed ( UserClickedEntity ent.id { x = ent.dx, y = ent.dy }, True )
            |> stopPropagationOn "click"
        , decodeCoords Coords
            |> Json.Decode.map (\c -> ( UserRightClickedEntity ent.id c, True ))
            |> preventDefaultOn "contextmenu"
        , UserHoversEntity ent.id
            |> onMouseOver
        , UserLeavesThing
            |> onMouseOut
        , translate (Entity.getX ent) (Entity.getY ent) |> transform
        , UserPushesLeftMouseButtonOnEntity ent.id
            |> Util.Graph.mousedown
        , Id.entityIdToString ent.id |> id
        ]
        [ rect
            [ width <| String.fromFloat <| Entity.getInnerWidth ent
            , height <| String.fromFloat <| Entity.getHeight ent
            , Css.entityRect vc
                ++ [ Css.fill color ]
                |> css
            , x rectX
            ]
            []
        , Svg.path
            [ isSelected
                |> Css.nodeFrame vc Model.Graph.Entity
                |> css
            , String.Interpolate.interpolate
                "M {2} 0 H {0} Z M {2} {1} H {0} Z"
                [ Entity.getInnerWidth ent + expandHandleWidth |> String.fromFloat
                , Entity.getHeight ent |> String.fromFloat
                , rectX
                ]
                |> d
            ]
            []
        , Svg.path
            [ Css.nodeSeparatorToExpandHandle vc Model.Graph.Entity |> css
            , String.Interpolate.interpolate
                "M {2} 0 V {0} Z M {1} 0 V {0} Z"
                [ Entity.getHeight ent |> String.fromFloat
                , Entity.getInnerWidth ent + expandHandleWidth |> String.fromFloat
                , rectX
                ]
                |> d
            ]
            []
        , label vc gc ent
        , flags plugins vc gc ent
        , currency vc gc ent
        , addressesCount vc gc ent
        , Node.expand vc
            gc
            { isOutgoing = False
            , nodeType = Model.Graph.Entity
            , degree = ent.entity.inDegree
            , onClick = UserClickedEntityExpandHandle ent.id
            , width = Entity.getInnerWidth ent
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
            , width = Entity.getInnerWidth ent
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
            + labelHeight
            / 2
            |> translate (Graph.padding * 2.5)
            |> transform
        ]
        [ getLabel vc gc ent
            |> Label.label vc gc
        ]


getLabel : Config -> Graph.Config -> Entity -> String
getLabel vc gc ent =
    ent.entity.bestAddressTag
        |> Maybe.andThen
            (\tag ->
                if String.isEmpty tag.label then
                    tag.category

                else
                    Just tag.label
            )
        |> Maybe.withDefault ""


flags : Plugins -> Config -> Graph.Config -> Entity -> Svg Msg
flags plugins vc gc ent =
    g
        [ Css.entityFlags vc |> css
        , Graph.padding
            * 1.5
            + labelHeight
            / 3
            |> translate (Graph.entityWidth - Graph.padding / 2)
            |> Util.Graph.scale 0.75
            |> transform
        ]
        (Plugin.View.Graph.Entity.flags plugins vc 0 ent)


currency : Config -> Graph.Config -> Entity -> Svg Msg
currency vc gc ent =
    g
        [ Css.entityCurrency vc |> css
        , (Graph.padding + labelHeight / 3.5)
            |> translate (Entity.getWidth ent - Graph.padding - expandHandleWidth)
            |> transform
        ]
        [ text_
            [ textAnchor "end"
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
        , ( UserClickedAddressesExpand ent.id, True )
            |> Json.Decode.succeed
            |> stopPropagationOn "click"
        ]
        [ text_
            [ textAnchor "middle"
            ]
            [ text string
            ]
        ]


addressLinks : Config -> Graph.Config -> String -> Float -> Float -> Entity -> Svg Msg
addressLinks vc gc selected mn mx ent =
    let
        _ =
            Log.log "Entity.addressLinks" ent.id
    in
    ent.addresses
        |> Dict.foldl
            (\_ address svg ->
                ( "addressLinks" ++ Id.addressIdToString address.id
                , Svg.lazy6 Address.links vc gc selected mn mx address
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


links : Config -> Graph.Config -> Float -> Float -> Entity -> Svg Msg
links vc gc mn mx ent =
    case ent.links of
        Entity.Links lnks ->
            lnks
                |> Dict.foldr
                    (\_ link svg ->
                        ( "entityLink" ++ (Id.entityLinkIdToString <| Id.initLinkId ent.id link.node.id)
                        , Svg.lazy6 Link.entityLink vc gc mn mx ent link
                        )
                            :: svg
                    )
                    []
                |> Keyed.node "g" []
