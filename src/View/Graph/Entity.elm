module View.Graph.Entity exposing (addressLinks, addressShadowLinks, addresses, entity, links, shadowLinks, showLink)

import Color
import Config.Graph as Graph exposing (expandHandleWidth, labelHeight)
import Config.View exposing (Config)
import Css
import Css.Graph as Css
import Dict exposing (Dict)
import Init.Graph.Id as Id
import Json.Decode
import Maybe.Extra
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Link exposing (Link)
import Model.Node exposing (NodeType(..))
import Msg.Graph exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Graph exposing (decodeCoords, translate)
import Util.View as Util
import View.Graph.Address as Address
import View.Graph.Label as Label
import View.Graph.Link as Link
import View.Graph.Node as Node
import View.Locale as Locale


addresses : Plugins -> Config -> Graph.Config -> Entity -> Svg Msg
addresses plugins vc gc ent =
    ent.addresses
        |> Dict.foldl
            (\_ address svg ->
                ( Id.addressIdToString address.id
                , Svg.lazy4 Address.address plugins vc gc address
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entity : Plugins -> Config -> Graph.Config -> Entity -> Svg Msg
entity plugins vc gc ent =
    let
        color =
            ent.color
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        ent.userTag
                            |> Maybe.andThen .category
                            |> Maybe.Extra.orElse ent.category
                            |> Maybe.map vc.theme.graph.categoryToColor
                            |> Maybe.withDefault vc.theme.graph.defaultColor
                            |> Color.toHsla
                            |> (\hsl ->
                                    { hsl
                                        | lightness = hsl.lightness * (vc.theme.graph.lightnessFactor vc.lightmode).entity
                                        , saturation = hsl.saturation * (vc.theme.graph.saturationFactor vc.lightmode).entity
                                    }
                               )
                            |> Color.fromHsla
                    )
                |> Util.toCssColor

        rectX =
            String.fromFloat expandHandleWidth
    in
    g
        [ Css.entityRoot vc gc.highlighter |> css
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
            [ ent.selected
                |> Css.nodeFrame vc EntityType
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
            [ Css.nodeSeparatorToExpandHandle vc EntityType |> css
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
            , nodeType = EntityType
            , degree = ent.entity.inDegree
            , onClick = UserClickedEntityExpandHandle ent.id
            , width = Entity.getInnerWidth ent
            , height = Entity.getHeight ent
            , color = color
            , isSelected = ent.selected
            }
        , Node.expand vc
            gc
            { isOutgoing = True
            , nodeType = EntityType
            , degree = ent.entity.outDegree
            , onClick = UserClickedEntityExpandHandle ent.id
            , width = Entity.getInnerWidth ent
            , height = Entity.getHeight ent
            , color = color
            , isSelected = ent.selected
            }
        ]


label : Config -> Graph.Config -> Entity -> Svg Msg
label vc gc ent =
    g
        [ Css.entityLabel vc |> css
        , Graph.entityPaddingTop
            |> translate (Graph.padding * 2.5)
            |> transform
        ]
        [ getLabel vc gc ent
            |> Label.label vc gc EntityType
        ]


getLabel : Config -> Graph.Config -> Entity -> String
getLabel vc gc ent =
    ent.userTag
        |> Maybe.map .label
        |> Maybe.Extra.orElseLazy
            (\_ ->
                ent.entity.bestAddressTag
                    |> Maybe.map
                        (\tag ->
                            Util.truncate gc.maxLettersPerLabelRow
                                (if not tag.tagpackIsPublic && String.isEmpty tag.label then
                                    "tag locked"

                                 else
                                    tag.label
                                )
                        )
            )
        |> Maybe.withDefault (String.fromInt ent.entity.entity)


addFlagsOffset : (Float -> List (Svg Msg)) -> List (Svg Msg) -> List (Svg Msg)
addFlagsOffset f acc =
    acc ++ f -((List.length acc |> toFloat) * 20.0)


flags : Plugins -> Config -> Graph.Config -> Entity -> Svg Msg
flags plugins vc gc ent =
    g
        [ Css.entityFlags vc |> css
        , Graph.entityPaddingTop
            + Graph.padding
            * 0.5
            + labelHeight
            / 2
            |> translate (Graph.entityWidth - Graph.padding / 2)
            |> Util.Graph.scale 0.75
            |> transform
        ]
        (List.foldl addFlagsOffset
            []
            [ tagsFlag vc ent
            , actorsFlag vc ent
            , \pluginOffsetStart ->
                Plugin.entityFlags plugins ent.plugins vc
                    |> (\( pluginOffset, pluginFlags ) ->
                            g [ translate (pluginOffsetStart - pluginOffset) 0 |> transform ]
                                pluginFlags
                       )
                    |> List.singleton
            ]
        )


tagsFlag : Config -> Entity -> Float -> List (Svg Msg)
tagsFlag vc ent offsetX =
    if ent.entity.noAddressTags > 0 then
        [ Svg.path
            [ translate offsetX 0
                |> Util.Graph.scale 0.033
                |> transform
            , Css.flag vc |> css
            , d "M48 32H197.5C214.5 32 230.7 38.74 242.7 50.75L418.7 226.7C443.7 251.7 443.7 292.3 418.7 317.3L285.3 450.7C260.3 475.7 219.7 475.7 194.7 450.7L18.75 274.7C6.743 262.7 0 246.5 0 229.5V80C0 53.49 21.49 32 48 32L48 32zM112 176C129.7 176 144 161.7 144 144C144 126.3 129.7 112 112 112C94.33 112 80 126.3 80 144C80 161.7 94.33 176 112 176z"
            , onClick <| UserClickedTagsFlag ent.id
            ]
            []
        ]

    else
        []


actorsFlag : Config -> Entity -> Float -> List (Svg Msg)
actorsFlag vc ent offsetX =
    if Entity.getActorsCount ent > 0 then
        [ Svg.path
            [ translate offsetX 0
                |> Util.Graph.scale 0.03
                |> transform
            , Css.flag vc |> css
            , d "M224 256A128 128 0 1 0 224 0a128 128 0 1 0 0 256zm-45.7 48C79.8 304 0 383.8 0 482.3C0 498.7 13.3 512 29.7 512H418.3c16.4 0 29.7-13.3 29.7-29.7C448 383.8 368.2 304 269.7 304H178.3z"
            ]
            (Entity.getActorsStr ent
                |> Maybe.map
                    (\x ->
                        Svg.text x
                            |> List.singleton
                            |> Svg.title []
                            |> List.singleton
                    )
                |> Maybe.withDefault []
            )
        ]

    else
        []


currency : Config -> Graph.Config -> Entity -> Svg Msg
currency vc gc ent =
    g
        [ Graph.entityPaddingTop
            |> translate (Entity.getWidth ent - Graph.padding - expandHandleWidth)
            |> transform
        ]
        [ text_
            [ textAnchor "end"
            , Css.entityCurrency vc |> css
            ]
            [ text (String.toUpper ent.entity.currency) ]
        ]


addressesCount : Config -> Graph.Config -> Entity -> Svg Msg
addressesCount vc gc ent =
    let
        total =
            Locale.int vc.locale ent.entity.noAddresses

        key =
            "{0} address"
                ++ (if ent.entity.noAddresses > 1 then
                        "es"

                    else
                        ""
                   )

        string =
            Locale.interpolated vc.locale key [ total ]
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


addressLinks : Config -> Graph.Config -> Float -> Float -> Entity -> Svg Msg
addressLinks vc gc mn mx ent =
    ent.addresses
        |> Dict.foldl
            (\_ address svg ->
                ( "addressLinks" ++ Id.addressIdToString address.id
                , Svg.lazy5 Address.links vc gc mn mx address
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


addressShadowLinks : Config -> Entity -> Svg Msg
addressShadowLinks vc ent =
    ent.addresses
        |> Dict.foldl
            (\_ address svg ->
                ( "addressShadowLinks" ++ Id.addressIdToString address.id
                , Svg.lazy2 Address.shadowLinks vc address
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
                        if showLink ent link then
                            ( "entityLink" ++ (Id.entityLinkIdToString <| Id.initLinkId ent.id link.node.id)
                            , Svg.lazy6 Link.entityLink vc gc mn mx ent link
                            )
                                :: svg

                        else
                            svg
                    )
                    []
                |> Keyed.node "g" []


linkHasAddressLinks : Dict Id.AddressId Address -> Dict Id.AddressId Address -> Bool
linkHasAddressLinks sourceAddresses targetAddresses =
    let
        checkAddressLinks lnks =
            case lnks of
                [] ->
                    False

                link :: rest ->
                    if Dict.member link targetAddresses then
                        True

                    else
                        checkAddressLinks rest

        checkAddresses addrs =
            case addrs of
                [] ->
                    False

                address :: rest ->
                    case address.links of
                        Address.Links lnks ->
                            if Dict.keys lnks |> checkAddressLinks then
                                True

                            else
                                checkAddresses rest
    in
    Dict.values sourceAddresses |> checkAddresses


shadowLinks : Config -> Entity -> Svg Msg
shadowLinks vc ent =
    case ent.shadowLinks of
        Entity.Links lnks ->
            lnks
                |> Dict.foldr
                    (\_ link svg ->
                        ( "entityShadowLink" ++ (Id.entityLinkIdToString <| Id.initLinkId ent.id link.node.id)
                        , Svg.lazy3 Link.entityShadowLink vc ent link
                        )
                            :: svg
                    )
                    []
                |> Keyed.node "g" []


showLink : Entity -> Link Entity -> Bool
showLink source target =
    target.forceShow || not (linkHasAddressLinks source.addresses target.node.addresses)
