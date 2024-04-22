module View.Pathfinder.Address exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import RemoteData
import Set
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Util.Graph exposing (translate)
import Util.View exposing (truncateLongIdentifier)


view : Plugins -> View.Config -> Pathfinder.Config -> Address -> Svg Msg
view _ vc _ address =
    let
        unit =
            View.getUnit vc
    in
    [ body vc
    , handles vc address
    , label vc address
    ]
        |> g
            [ translate (address.x * unit) (address.y * unit)
                |> transform
            , Css.address vc |> css
            , UserClickedAddress address.id |> onClick
            ]


body : View.Config -> Svg Msg
body vc =
    circle
        [ cx "0"
        , cy "0"
        , r <| String.fromFloat vc.theme.pathfinder.addressRadius
        ]
        []


handles : View.Config -> Address -> Svg Msg
handles vc address =
    let
        data =
            RemoteData.toMaybe address.data

        nonZero field =
            data
                |> Maybe.map field
                |> Maybe.map ((<) 0)
                |> Maybe.withDefault False
    in
    (if nonZero .noIncomingTxs && Set.isEmpty address.incomingTxs then
        [ handle vc address.id Incoming
        ]

     else
        []
    )
        ++ (if nonZero .noOutgoingTxs && Set.isEmpty address.outgoingTxs then
                [ handle vc address.id Outgoing
                ]

            else
                []
           )
        |> g []


handle : View.Config -> Id -> Direction -> Svg Msg
handle vc id direction =
    let
        unit =
            View.getUnit vc

        len =
            unit / 6

        offsetX =
            case direction of
                Incoming ->
                    -unit

                Outgoing ->
                    unit
    in
    [ line
        [ x1 "0"
        , x2 "0"
        , y1 <| String.fromFloat -len
        , y2 <| String.fromFloat len
        ]
        []
    , line
        [ x1 <| String.fromFloat -len
        , x2 <| String.fromFloat len
        , y1 "0"
        , y2 "0"
        ]
        []
    , circle
        [ cx "0"
        , cy "0"
        , r <| String.fromFloat len
        , css
            [ Css.property "fill" "transparent"
            , Css.property "stroke" "transparent"
            , Css.cursor Css.pointer
            ]
        ]
        []
    ]
        |> g
            [ translate (offsetX / 1.3) 0
                |> transform
            , UserClickedAddressExpandHandle id direction |> onClick
            , Json.Decode.succeed ( NoOp, True )
                |> stopPropagationOn "mousedown"
            , Css.addressHandle vc |> css
            ]


label : View.Config -> Address -> Svg Msg
label vc { id } =
    let
        tx =
            0

        ty =
            vc.theme.pathfinder.addressRadius
                + vc.theme.pathfinder.addressSpacingToLabel
    in
    id
        |> Id.id
        |> truncateLongIdentifier
        |> text
        |> List.singleton
        |> tspan
            [ alignmentBaseline "hanging"
            ]
        |> List.singleton
        |> text_
            [ tx |> String.fromFloat |> x
            , ty |> String.fromFloat |> y
            , textAnchor "middle"
            , Css.addressLabel vc |> css
            ]


onClick : Msg -> Attribute Msg
onClick msg =
    Json.Decode.succeed ( msg, True )
        |> stopPropagationOn "click"
