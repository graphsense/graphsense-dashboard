module View.Pathfinder.Address exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (..)
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import RemoteData
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Util.Graph exposing (translate)


view : Plugins -> View.Config -> Pathfinder.Config -> Address -> Svg Msg
view _ vc _ address =
    let
        unit =
            View.getUnit vc
    in
    [ body vc
    , handles vc address
    ]
        |> g
            [ translate (address.x * unit * 2) (address.y * unit * 2)
                |> transform
            , Css.address vc |> css
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
    (if nonZero .noIncomingTxs then
        [ handle vc address.id Incoming
        ]

     else
        []
    )
        ++ (if nonZero .noOutgoingTxs then
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


onClick : Msg -> Attribute Msg
onClick msg =
    Json.Decode.succeed ( msg, True )
        |> stopPropagationOn "click"
