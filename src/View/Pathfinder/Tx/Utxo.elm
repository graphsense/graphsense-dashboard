module View.Pathfinder.Tx.Utxo exposing (edge, view)

import Animation as A
import Color
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Dict
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Json.Decode
import Maybe.Extra
import Model.Currency exposing (Currency(..))
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
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents exposing (txNodeUtxoAttributes)
import Theme.Svg.Icons as Icons
import Tuple exposing (pair, second)
import Util.Annotations as Annotations exposing (annotationToAttrAndLabel)
import Util.Data as Data
import Util.Graph exposing (decodeCoords, translate)
import Util.View exposing (ifTrue, onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (labelsSep, pickPathFunction)
import View.Pathfinder.Tx.Utils exposing (signX, toPosition)


view : Plugins -> View.Config -> Pathfinder.Config -> Tx -> UtxoTx -> Maybe Annotations.AnnotationItem -> Svg Msg
view _ vc _ tx utxo annotation =
    let
        id =
            tx.id

        colorFinal =
            annotation
                |> Maybe.andThen .color
                |> Maybe.map Color.toCssString
                |> Maybe.Extra.or
                    (case tx.conversionType of
                        Just InputLegConversion ->
                            Just Colors.pathIn

                        Just OutputLegConversion ->
                            Just Colors.pathOut

                        Nothing ->
                            Nothing
                    )
                |> Maybe.withDefault Colors.pathMiddle

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
                + offsetTxHash

        offsetTxHash =
            if vc.showHash then
                0

            else
                -GraphComponents.txNodeUtxoTxHash_details.renderedHeight

        t =
            Data.timestampToPosix utxo.raw.timestamp

        ( annAttr, label ) =
            annotation
                |> Maybe.map
                    (annotationToAttrAndLabel
                        tx
                        GraphComponents.txNodeUtxo_details
                        offset
                        UserOpensTxAnnotationDialog
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
                | root =
                    [ UserClickedTx id |> onClickWithStop
                    , UserPushesLeftMouseButtonOnUtxoTx id
                        |> Util.Graph.mousedown
                    , UserMovesMouseOverTx id
                        |> onMouseOver
                    , UserMovesMouseOutTx id
                        |> onMouseLeave
                    , css [ Css.cursor Css.pointer ]
                    , Id.toString id
                        |> Svg.Styled.Attributes.id
                    , decodeCoords Coords.Coords
                        |> Json.Decode.map (\c -> ( UserOpensContextMenu c (ContextMenu.TransactionContextMenu id), True ))
                        |> preventDefaultOn "contextmenu"
                    ]
                , txNode = annAttr
                , highlightEllipse = [ Css.property "stroke" colorFinal |> Css.important ] |> css |> List.singleton
                , date =
                    [ translate 0 offsetTxHash |> transform ]
                , time =
                    [ translate 0 offsetTxHash |> transform ]
            }
            { root =
                { hasMultipleInOutputs = anyIsNotVisible utxo.inputs || anyIsNotVisible utxo.outputs
                , highlightVisible = tx.selected || tx.hovered
                , txHash = Util.View.truncateLongIdentifier utxo.raw.txHash |> ifTrue vc.showHash
                , date = Locale.timestampDateUniform vc.locale t |> ifTrue vc.showTimestampOnTxEdge
                , time = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset t |> ifTrue vc.showTimestampOnTxEdge
                , timestampVisible = vc.showTimestampOnTxEdge || vc.showHash
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


edge : Plugins -> View.Config -> Pathfinder.Config -> UtxoTx -> Tx -> Maybe Annotations.AnnotationItem -> Svg Msg
edge _ vc _ utxo tx annotation =
    let
        assetToValue asset =
            let
                fmt c =
                    Locale.currency c vc.locale asset
            in
            String.join labelsSep <|
                -- join to make lazyable
                if vc.showBothValues then
                    [ fmt Coin
                    , fmt (Fiat vc.preferredFiatCurrency)
                    ]

                else
                    [ fmt (View.toCurrency vc) ]

        toValues =
            Dict.toList
                >> List.filterMap
                    (\( id, { values, address } ) ->
                        address
                            |> Maybe.map
                                (values
                                    |> pair { network = Id.network id, asset = Id.network id }
                                    |> List.singleton
                                    |> assetToValue
                                    |> pair
                                )
                    )

        outputValues =
            utxo.outputs
                |> toValues

        inputValues =
            utxo.inputs
                |> toValues

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2

        txRad =
            GraphComponents.txNodeUtxoTxNode_details.width / 2

        txPos =
            tx |> toPosition

        txId =
            Id.init utxo.raw.currency utxo.raw.txHash

        color =
            annotation
                |> Maybe.andThen .color
                |> Maybe.map Color.toCssString

        isConversionLeg =
            Maybe.Extra.isJust tx.conversionType

        colorFinal =
            color
                |> Maybe.Extra.or
                    (tx.conversionType
                        |> Maybe.map
                            (\ct ->
                                case ct of
                                    InputLegConversion ->
                                        Colors.pathIn

                                    OutputLegConversion ->
                                        Colors.pathOut
                            )
                    )
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
                , pickPathFunction False
                    (tx.hovered || tx.selected)
                    colorFinal
                    isConversionLeg
                    values
                    (fromPos.x * unit + (rad * sign))
                    (fromPos.y * unit)
                    (txPos.x * unit - (txRad * sign))
                    (txPos.y * unit)
                    (A.animate tx.clock tx.opacity)
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
                        , pickPathFunction True
                            (tx.hovered || tx.selected)
                            colorFinal
                            isConversionLeg
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
                |> UserMovesMouseOverTx
                |> onMouseOver
            , txId
                |> UserMovesMouseOutTx
                |> onMouseLeave
            , txId
                |> UserClickedTx
                |> onClickWithStop
            ]
