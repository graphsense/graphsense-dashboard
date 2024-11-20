module View.Pathfinder.Tx.Utxo exposing (edge, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Dict
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Graph.Coords as Coords
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Theme.Svg.GraphComponents as GraphComponents exposing (txNodeUtxoAttributes)
import Theme.Svg.Icons as Icons
import Tuple exposing (pair, second)
import Util.Annotations as Annotations exposing (annotationToAttrAndLabel)
import Util.Graph exposing (decodeCoords, translate)
import Util.View exposing (onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (inPath, inPathHovered, outPath, outPathHovered)
import View.Pathfinder.Tx.Utils exposing (AnimatedPosTrait, signX, toPosition)


view : Plugins -> View.Config -> Pathfinder.Config -> Tx -> UtxoTx -> Maybe Annotations.AnnotationItem -> Svg Msg
view _ vc _ tx utxo annotation =
    let
        id =
            tx.id

        anyIsNotVisible =
            Dict.toList
                >> List.any (second >> .address >> (==) Nothing)

        fd =
            GraphComponents.txNodeUtxoTxNode_details

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2

        offset =
            2
                + (if vc.showTimestampOnTxEdge then
                    0

                   else
                    -GraphComponents.txNodeUtxoTxText_details.height
                  )

        ( annAttr, label ) =
            annotation
                |> Maybe.map
                    (annotationToAttrAndLabel
                        tx
                        GraphComponents.txNodeUtxo_details
                        offset
                    )
                |> Maybe.withDefault ( [], [] )
    in
    g
        [ translate
            ((tx.x + tx.dx) * unit - adjX)
            ((A.animate tx.clock tx.y + tx.dy) * unit - adjY)
            |> transform
        , A.animate tx.clock tx.opacity
            |> String.fromFloat
            |> opacity
        ]
        (GraphComponents.txNodeUtxoWithAttributes
            { txNodeUtxoAttributes
                | txNodeUtxo =
                    [ UserClickedTx id |> onClickWithStop
                    , UserPushesLeftMouseButtonOnUtxoTx id
                        |> Util.Graph.mousedown
                    , UserMovesMouseOverUtxoTx id
                        |> onMouseOver
                    , UserMovesMouseOutUtxoTx id
                        |> onMouseLeave
                    , css [ Css.cursor Css.pointer ]
                    , Id.toString id
                        |> Svg.Styled.Attributes.id
                    , decodeCoords Coords.Coords
                        |> Json.Decode.map (\c -> ( UserOpensContextMenu c (ContextMenu.TransactionContextMenu id), True ))
                        |> preventDefaultOn "contextmenu"
                    ]
                , txNode = annAttr
            }
            { txNodeUtxo =
                { hasMultipleInOutputs = anyIsNotVisible utxo.inputs || anyIsNotVisible utxo.outputs
                , highlightVisible = tx.selected || tx.hovered
                , date = Locale.timestampDateUniform vc.locale utxo.raw.timestamp
                , time = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset utxo.raw.timestamp
                , timestampVisible = vc.showTimestampOnTxEdge
                , startingPointVisible = tx.isStartingPoint || tx.selected
                }
            , iconsNodeMarker =
                { variant =
                    case ( tx.selected, tx.isStartingPoint ) of
                        ( True, _ ) ->
                            Icons.iconsNodeMarkerPurposeSelectedNode {}

                        ( False, False ) ->
                            text ""

                        ( False, True ) ->
                            Icons.iconsNodeMarkerPurposeStartingPoint {}
                }
            }
            :: label
        )


edge : Plugins -> View.Config -> Pathfinder.Config -> Bool -> UtxoTx -> AnimatedPosTrait x -> Svg Msg
edge _ vc _ hovered tx pos =
    let
        toValues =
            Dict.toList
                >> List.filterMap
                    (\( id, { values, address } ) ->
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
                                    |> pair
                                )
                    )

        outputValues =
            tx.outputs
                |> toValues

        inputValues =
            tx.inputs
                |> toValues

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2

        txRad =
            GraphComponents.txNodeUtxoTxNode_details.width / 2

        txPos =
            pos |> toPosition

        txId =
            Id.init tx.raw.currency tx.raw.txHash
    in
    (inputValues
        |> List.map
            (\( values, address ) ->
                let
                    fromPos =
                        address |> toPosition

                    sign =
                        signX fromPos txPos
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
                    (fromPos.x * unit + (rad * sign))
                    (fromPos.y * unit)
                    (txPos.x * unit - (txRad * sign))
                    (txPos.y * unit)
                    (A.animate pos.clock pos.opacity)
                )
            )
    )
        ++ (outputValues
                |> List.map
                    (\( values, address ) ->
                        let
                            toPos =
                                address |> toPosition

                            sign =
                                signX txPos toPos
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
                            (txPos.x * unit + (txRad * sign))
                            (txPos.y * unit)
                            (toPos.x * unit - (rad * sign))
                            (toPos.y * unit)
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
