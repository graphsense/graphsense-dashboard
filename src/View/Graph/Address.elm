module View.Graph.Address exposing (address)

import Color
import Config.Graph as Graph exposing (AddressLabelType(..), labelHeight)
import Config.View exposing (Config)
import Css
import Css.Graph as Css
import Dict
import Json.Decode
import Log
import Model.Graph
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Id as Id
import Msg.Graph exposing (Msg(..))
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (..)
import Svg.Styled.Lazy as Svg exposing (..)
import Util.Graph exposing (translate)
import Util.View as Util
import View.Graph.Label as Label
import View.Graph.Node as Node
import View.Locale as Locale


address : Config -> Graph.Config -> Id.AddressId -> Address -> Svg Msg
address vc gc selected addr =
    let
        _ =
            Log.log "rednerAddress" addr.id

        color =
            addr.category
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
                |> Util.toCssColor

        isSelected =
            selected == addr.id
    in
    g
        [ Css.addressRoot vc |> css
        , Json.Decode.succeed ( UserClickedAddress addr.id, True )
            |> stopPropagationOn "click"
        , UserRightClickedAddress addr.id
            |> Json.Decode.succeed
            |> on "contextmenu"
        , UserHoversAddress addr.id
            |> onMouseOver
        , UserLeavesThing
            |> onMouseOut
        , translate (addr.x + addr.dx) (addr.y + addr.dy)
            |> transform
        , UserPushesLeftMouseButtonOnEntity addr.entityId
            |> Util.Graph.mousedown
        ]
        [ rect
            [ width <| String.fromFloat Graph.addressWidth
            , height <| String.fromFloat Graph.addressHeight
            , Css.addressRect vc
                ++ [ Css.fill color ]
                |> css
            ]
            []
        , Svg.path
            [ Css.nodeFrame vc Model.Graph.Address isSelected |> css
            , String.Interpolate.interpolate
                "M 0 0 H {0} Z M 0 {1} H {0} Z"
                [ Address.getWidth addr |> String.fromFloat
                , Address.getHeight addr |> String.fromFloat
                ]
                |> d
            ]
            []
        , Svg.path
            [ Css.nodeSeparatorToExpandHandle vc Model.Graph.Address |> css
            , String.Interpolate.interpolate
                "M 0 0 V {0} Z M {1} 0 V {0} Z"
                [ Address.getHeight addr |> String.fromFloat
                , Address.getWidth addr |> String.fromFloat
                ]
                |> d
            ]
            []
        , label vc gc addr
        , flags vc gc addr
        , Node.expand vc
            gc
            { isOutgoing = False
            , nodeType = Model.Graph.Address
            , degree = addr.address.inDegree
            , onClick = UserClickedAddressExpandHandle addr.id
            , width = Address.getWidth addr
            , height = Address.getHeight addr
            , color = color
            , isSelected = isSelected
            }
        , Node.expand vc
            gc
            { isOutgoing = True
            , nodeType = Model.Graph.Address
            , degree = addr.address.outDegree
            , onClick = UserClickedAddressExpandHandle addr.id
            , width = Address.getWidth addr
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
            + labelHeight
            / 3
            |> translate Graph.padding
            |> transform
        ]
        [ getLabel vc gc addr
            |> Label.label vc gc
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
            "todo"


flags : Config -> Graph.Config -> Address -> Svg Msg
flags vc gc addr =
    g
        [ Css.addressFlags vc |> css
        , Graph.padding
            / 2
            |> translate (Graph.addressWidth - Graph.padding / 2)
            |> transform
        ]
        []
