module View.Graph.Address exposing (address, links, shadowLinks)

--import Plugin.View.Graph.Address

import Color
import Config.Graph as Graph exposing (AddressLabelType(..), expandHandleWidth, labelHeight)
import Config.View exposing (Config)
import Css
import Css.Graph as Css
import Dict
import Init.Graph.Id as Id
import Json.Decode
import Log
import Maybe.Extra
import Model.Graph
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Coords as Coords exposing (Coords)
import Model.Graph.Id as Id
import Msg.Graph exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Route
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg exposing (..)
import Util.Graph exposing (decodeCoords, translate)
import Util.View as Util
import View.Graph.Label as Label
import View.Graph.Link as Link
import View.Graph.Node as Node
import View.Locale as Locale


address : Plugins -> Config -> Graph.Config -> Address -> Svg Msg
address plugins vc gc addr =
    let
        _ =
            Log.log "rednerAddress" addr.id

        color =
            addr.color
                |> Maybe.Extra.withDefaultLazy
                    (\_ ->
                        addr.userTag
                            |> Maybe.andThen .category
                            |> Maybe.Extra.orElse addr.category
                            |> Maybe.andThen
                                (\category -> Dict.get category gc.colors)
                            |> Maybe.withDefault vc.theme.graph.defaultColor
                            |> Color.toHsla
                            |> (\hsl ->
                                    { hsl
                                        | lightness = hsl.lightness * vc.theme.graph.lightnessFactor.address
                                        , saturation = hsl.saturation * vc.theme.graph.saturationFactor.address
                                    }
                               )
                            |> Color.fromHsla
                    )
                |> Util.toCssColor

        isSelected =
            addr.selected

        rectX =
            String.fromFloat expandHandleWidth
    in
    g
        [ Css.addressRoot vc gc.highlighter |> css
        , Json.Decode.succeed ( UserClickedAddress addr.id, True )
            |> stopPropagationOn "click"
        , decodeCoords Coords
            |> Json.Decode.map (\c -> ( UserRightClickedAddress addr.id c, True ))
            |> preventDefaultOn "contextmenu"
        , UserHoversAddress addr.id
            |> onMouseOver
        , UserLeavesThing
            |> onMouseOut
        , translate (Address.getX addr) (Address.getY addr)
            |> transform
        , UserPushesLeftMouseButtonOnEntity addr.entityId
            |> Util.Graph.mousedown
        , Id.addressIdToString addr.id
            |> id
        ]
        [ rect
            [ width <| String.fromFloat Graph.addressWidth
            , height <| String.fromFloat Graph.addressHeight
            , Css.addressRect vc
                ++ [ Css.fill color ]
                |> css
            , x rectX
            ]
            []
        , Svg.path
            [ Css.nodeFrame vc Model.Graph.AddressType isSelected |> css
            , String.Interpolate.interpolate
                "M {2} 0 H {0} Z M {2} {1} H {0} Z"
                [ Address.getInnerWidth addr + expandHandleWidth |> String.fromFloat
                , Address.getHeight addr |> String.fromFloat
                , rectX
                ]
                |> d
            ]
            []
        , Svg.path
            [ Css.nodeSeparatorToExpandHandle vc Model.Graph.AddressType |> css
            , String.Interpolate.interpolate
                "M {2} 0 V {0} Z M {1} 0 V {0} Z"
                [ Address.getHeight addr |> String.fromFloat
                , Address.getInnerWidth addr + expandHandleWidth |> String.fromFloat
                , rectX
                ]
                |> d
            ]
            []
        , label vc gc addr
        , flags plugins vc gc addr
        , Node.expand vc
            gc
            { isOutgoing = False
            , nodeType = Model.Graph.AddressType
            , degree = addr.address.inDegree
            , onClick = UserClickedAddressExpandHandle addr.id
            , width = Address.getInnerWidth addr
            , height = Address.getHeight addr
            , color = color
            , isSelected = isSelected
            }
        , Node.expand vc
            gc
            { isOutgoing = True
            , nodeType = Model.Graph.AddressType
            , degree = addr.address.outDegree
            , onClick = UserClickedAddressExpandHandle addr.id
            , width = Address.getInnerWidth addr
            , height = Address.getHeight addr
            , color = color
            , isSelected = isSelected
            }
        ]


label : Config -> Graph.Config -> Address -> Svg Msg
label vc gc addr =
    g
        [ Css.addressLabel vc |> css
        , Graph.addressHeight
            / 2
            |> translate (Graph.padding + expandHandleWidth)
            |> transform
        ]
        [ getLabel vc gc addr
            |> Label.label vc
                gc
                Model.Graph.AddressType
        ]


getLabel : Config -> Graph.Config -> Address -> String
getLabel vc gc addr =
    case gc.addressLabelType of
        ID ->
            addr.address.address
                |> String.left 8

        Balance ->
            addr.address.balance
                |> Locale.currency vc.locale (Id.currency addr.id)

        Tag ->
            addr.userTag
                |> Maybe.map .label
                |> Maybe.Extra.orElseLazy
                    (\_ ->
                        Address.bestTag addr.tags
                            |> Maybe.map
                                (\tag ->
                                    if not tag.tagpackIsPublic && String.isEmpty tag.label then
                                        "tag locked"

                                    else
                                        tag.label
                                )
                    )
                |> Maybe.withDefault (addr.address.address |> String.left 8)


flags : Plugins -> Config -> Graph.Config -> Address -> Svg Msg
flags plugins vc gc addr =
    let
        af =
            abuseFlag vc addr

        offset =
            if List.isEmpty af then
                0

            else
                15
    in
    af
        ++ contractFlag vc addr
        ++ [ Plugin.addressFlags plugins addr.plugins vc
                |> (\( pluginOffset, pluginFlags ) ->
                        g [ translate (-offset - pluginOffset) 0 |> transform ]
                            pluginFlags
                   )
                |> Log.log "View.Graph.Address flags result"
           ]
        |> g
            [ Css.addressFlags vc |> css
            , Graph.padding
                / 2
                |> translate Graph.addressWidth
                |> Util.Graph.scale 0.75
                |> transform
            ]


abuseFlag : Config -> Address -> List (Svg Msg)
abuseFlag vc addr =
    let
        hasAbuse =
            .abuse >> Maybe.map (\_ -> True)
    in
    addr.userTag
        |> Maybe.andThen hasAbuse
        |> Maybe.Extra.orElseLazy
            (\_ ->
                case addr.tags |> Maybe.map (List.any (hasAbuse >> Maybe.withDefault False)) of
                    Just True ->
                        Just True

                    _ ->
                        Nothing
            )
        |> Maybe.map
            (\_ ->
                [ Svg.path
                    [ translate 5 0
                        |> Util.Graph.scale 0.03
                        |> transform
                    , Css.abuseFlag vc |> css
                    , d "M296 160H180.6l42.6-129.8C227.2 15 215.7 0 200 0H56C44 0 33.8 8.9 32.2 20.8l-32 240C-1.7 275.2 9.5 288 24 288h118.7L96.6 482.5c-3.6 15.2 8 29.5 23.3 29.5 8.4 0 16.4-4.4 20.8-12l176-304c9.3-15.9-2.2-36-20.7-36z"
                    ]
                    []
                ]
            )
        |> Maybe.withDefault []


contractFlag : Config -> Address -> List (Svg Msg)
contractFlag vc addr =
    if addr.address.isContract == Just True then
        [ Svg.path
            [ translate 0 0
                |> Util.Graph.scale 0.03
                |> transform
            , Css.flag vc |> css
            , d "M495.9 166.6c3.2 8.7 .5 18.4-6.4 24.6l-43.3 39.4c1.1 8.3 1.7 16.8 1.7 25.4s-.6 17.1-1.7 25.4l43.3 39.4c6.9 6.2 9.6 15.9 6.4 24.6c-4.4 11.9-9.7 23.3-15.8 34.3l-4.7 8.1c-6.6 11-14 21.4-22.1 31.2c-5.9 7.2-15.7 9.6-24.5 6.8l-55.7-17.7c-13.4 10.3-28.2 18.9-44 25.4l-12.5 57.1c-2 9.1-9 16.3-18.2 17.8c-13.8 2.3-28 3.5-42.5 3.5s-28.7-1.2-42.5-3.5c-9.2-1.5-16.2-8.7-18.2-17.8l-12.5-57.1c-15.8-6.5-30.6-15.1-44-25.4L83.1 425.9c-8.8 2.8-18.6 .3-24.5-6.8c-8.1-9.8-15.5-20.2-22.1-31.2l-4.7-8.1c-6.1-11-11.4-22.4-15.8-34.3c-3.2-8.7-.5-18.4 6.4-24.6l43.3-39.4C64.6 273.1 64 264.6 64 256s.6-17.1 1.7-25.4L22.4 191.2c-6.9-6.2-9.6-15.9-6.4-24.6c4.4-11.9 9.7-23.3 15.8-34.3l4.7-8.1c6.6-11 14-21.4 22.1-31.2c5.9-7.2 15.7-9.6 24.5-6.8l55.7 17.7c13.4-10.3 28.2-18.9 44-25.4l12.5-57.1c2-9.1 9-16.3 18.2-17.8C227.3 1.2 241.5 0 256 0s28.7 1.2 42.5 3.5c9.2 1.5 16.2 8.7 18.2 17.8l12.5 57.1c15.8 6.5 30.6 15.1 44 25.4l55.7-17.7c8.8-2.8 18.6-.3 24.5 6.8c8.1 9.8 15.5 20.2 22.1 31.2l4.7 8.1c6.1 11 11.4 22.4 15.8 34.3zM256 336c44.2 0 80-35.8 80-80s-35.8-80-80-80s-80 35.8-80 80s35.8 80 80 80z"
            ]
            []
        ]

    else
        []


links : Config -> Graph.Config -> Float -> Float -> Address -> Svg Msg
links vc gc mn mx addr =
    case addr.links of
        Address.Links lnks ->
            lnks
                |> Dict.foldr
                    (\_ link svg ->
                        ( "addressLink" ++ (Id.addressLinkIdToString <| Id.initLinkId addr.id link.node.id)
                        , Svg.lazy6 Link.addressLink vc gc mn mx addr link
                        )
                            :: svg
                    )
                    []
                |> Keyed.node "g" []


shadowLinks : Config -> Address -> Svg Msg
shadowLinks vc addr =
    case addr.shadowLinks of
        Address.Links lnks ->
            lnks
                |> Dict.foldr
                    (\_ link svg ->
                        ( "addressShadowLink" ++ (Id.addressLinkIdToString <| Id.initLinkId addr.id link.node.id)
                        , Svg.lazy3 Link.addressShadowLink vc addr link
                        )
                            :: svg
                    )
                    []
                |> Keyed.node "g" []
