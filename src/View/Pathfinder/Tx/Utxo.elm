module View.Pathfinder.Tx.Utxo exposing (edge, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Dict exposing (Dict)
import Dict.Nonempty as NDict
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Svg.PathD exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Tuple exposing (pair, second)
import Util.Graph exposing (translate)
import Util.Pathfinder exposing (getAddress)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (inPath, outPath)


view : Plugins -> View.Config -> Pathfinder.Config -> Id -> UtxoTx -> Svg Msg
view _ vc _ id tx =
    let
        unit =
            View.getUnit vc

        anyIsNotVisible =
            NDict.toList
                >> List.any (second >> .visible >> not)
    in
    body vc
        :: (if anyIsNotVisible tx.inputs || anyIsNotVisible tx.outputs then
                [ moreIndicator vc
                ]

            else
                []
           )
        |> g
            [ translate
                ((tx.x + tx.dx) * unit)
                ((A.animate tx.clock tx.y + tx.dy) * unit)
                |> transform
            , A.animate tx.clock tx.opacity
                |> String.fromFloat
                |> opacity
            , Css.tx vc |> css
            , UserClickedTx id |> onClick
            , UserPushesLeftMouseButtonOnUtxoTx id
                |> Util.Graph.mousedown
            ]


moreIndicator : View.Config -> Svg Msg
moreIndicator vc =
    let
        dot attrs =
            circle
                (attrs
                    ++ [ vc.theme.pathfinder.txRadius
                            / 3
                            |> String.fromFloat
                            |> r
                       ]
                )
                []
    in
    g
        [ translate 0 (vc.theme.pathfinder.txRadius * 1.5)
            |> transform
        ]
        [ dot
            [ cx "0"
            , cy "0"
            ]
        , dot
            [ cy "0"
            , vc.theme.pathfinder.txRadius
                / 1.5
                |> String.fromFloat
                |> cx
            ]
        , dot
            [ cy "0"
            , -vc.theme.pathfinder.txRadius
                / 1.5
                |> String.fromFloat
                |> cx
            ]
        ]


body : View.Config -> Svg Msg
body vc =
    circle
        [ cx "0"
        , cy "0"
        , r <| String.fromFloat vc.theme.pathfinder.txRadius
        ]
        []


edge : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> UtxoTx -> Svg Msg
edge _ vc _ addresses tx =
    let
        toValues =
            NDict.toList
                >> List.filterMap
                    (\( id, { values } ) ->
                        getAddress addresses id
                            |> Result.toMaybe
                            |> Maybe.map
                                (values
                                    |> pair { network = Id.network id, asset = Id.network id }
                                    |> List.singleton
                                    |> Locale.currency vc.locale
                                    >> pair
                                )
                    )

        outputValues =
            tx.outputs
                |> toValues

        inputValues =
            tx.inputs
                |> toValues

        unit =
            View.getUnit vc

        rad =
            vc.theme.pathfinder.addressRadius

        txRad =
            vc.theme.pathfinder.txRadius

        toCoords address =
            { tx = tx.x + tx.dx
            , ty = A.animate tx.clock tx.y + tx.dy
            , ax = address.x + address.dx
            , ay = A.animate address.clock address.y + address.dy
            }
    in
    (inputValues
        |> List.map
            (\( values, address ) ->
                let
                    c =
                        toCoords address

                    sign =
                        if c.ax > c.tx then
                            -1

                        else
                            1
                in
                ( Id.toString address.id
                , Svg.lazy7 inPath
                    vc
                    values
                    (c.ax * unit + (rad * sign))
                    (c.ay * unit)
                    (c.tx * unit - (txRad * sign))
                    (c.ty * unit)
                    (A.animate tx.clock tx.opacity)
                )
            )
    )
        ++ (outputValues
                |> List.map
                    (\( values, address ) ->
                        let
                            c =
                                toCoords address

                            sign =
                                if c.ax < c.tx then
                                    -1

                                else
                                    1
                        in
                        ( Id.toString address.id
                        , Svg.lazy7 outPath
                            vc
                            values
                            (c.tx * unit + (txRad * sign))
                            (c.ty * unit)
                            (c.ax * unit - (rad * sign))
                            (c.ay * unit)
                            (A.animate address.clock address.opacity)
                        )
                    )
           )
        |> Keyed.node "g"
            []
