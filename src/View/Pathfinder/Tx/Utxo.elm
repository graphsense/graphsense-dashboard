module View.Pathfinder.Tx.Utxo exposing (edge, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Dict
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (unit)
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
import Theme.Svg.GraphComponents as GraphComponents exposing (txNodeCircleAttributes)
import Tuple exposing (pair, second)
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (inPath, inPathHovered, outPath, outPathHovered)


view : Plugins -> View.Config -> Pathfinder.Config -> Id -> Bool -> UtxoTx -> Svg Msg
view _ vc pc id highlight tx =
    let
        anyIsNotVisible =
            Dict.toList
                >> List.any (second >> .address >> (==) Nothing)

        fd =
            GraphComponents.txNodeCircleTxNodeDetails

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2
    in
    GraphComponents.txNodeCircleWithAttributes
        { txNodeCircleAttributes
            | txNodeCircle =
                [ translate
                    ((tx.x + tx.dx) * unit - adjX)
                    ((A.animate tx.clock tx.y + tx.dy) * unit - adjY)
                    |> transform
                , A.animate tx.clock tx.opacity
                    |> String.fromFloat
                    |> opacity
                , UserClickedTx id |> onClickWithStop
                , UserPushesLeftMouseButtonOnUtxoTx id
                    |> Util.Graph.mousedown
                , UserMovesMouseOverUtxoTx id
                    |> onMouseOver
                , UserMovesMouseOutUtxoTx id
                    |> onMouseLeave
                , css [ Css.cursor Css.pointer ]
                , Id.toString id
                    |> Svg.Styled.Attributes.id
                ]
        }
        { txNodeCircle =
            { multipleInOutputs = anyIsNotVisible tx.inputs || anyIsNotVisible tx.outputs
            , highlight = highlight
            , date = Locale.timestampDateUniform vc.locale tx.raw.timestamp
            , time = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset tx.raw.timestamp
            , timestamp = pc.showTxTimestamps
            }
        }


edge : Plugins -> View.Config -> Pathfinder.Config -> Bool -> UtxoTx -> Svg Msg
edge _ vc _ hovered tx =
    let
        toValues =
            Dict.toList
                >> List.filterMap
                    (\( id, { values, aggregatesN, address } ) ->
                        address
                            |> Maybe.map
                                (values
                                    |> pair { network = Id.network id, asset = Id.network id }
                                    |> List.singleton
                                    |> Locale.currency vc.locale
                                    --|> (\x ->
                                    --        if aggregatesN > 1 then
                                    --            x ++ " (" ++ String.fromInt aggregatesN ++ ")"
                                    --        else
                                    --            x
                                    --   )
                                    >> pair
                                )
                    )

        outputValues =
            tx.outputs
                |> toValues

        inputValues =
            tx.inputs
                |> toValues

        fd =
            GraphComponents.addressNodeNodeFrameDetails

        rad =
            fd.width / 2

        txRad =
            vc.theme.pathfinder.txRadius

        toCoords address =
            { tx = tx.x + tx.dx
            , ty = A.animate tx.clock tx.y + tx.dy
            , ax = address.x + address.dx
            , ay = A.animate address.clock address.y + address.dy
            }

        txId =
            Id.init tx.raw.currency tx.raw.txHash
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
                , Svg.lazy7
                    (if hovered then
                        inPathHovered

                     else
                        inPath
                    )
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
                        , Svg.lazy7
                            (if hovered then
                                outPathHovered

                             else
                                outPath
                            )
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
            [ txId
                |> UserMovesMouseOverUtxoTx
                |> onMouseOver
            , txId
                |> UserMovesMouseOutUtxoTx
                |> onMouseLeave
            , txId
                |> UserClickedTx
                |> onClickWithStop
            ]
