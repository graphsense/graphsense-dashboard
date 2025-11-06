module View.Pathfinder.Tx.AccountTx exposing (edge, view)

import Animation as A
import Color
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Json.Decode
import Maybe.Extra
import Model.Currency exposing (asset)
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
import Theme.Svg.GraphComponents as GraphComponents exposing (txNodeEthAttributes)
import Theme.Svg.Icons as Icons
import Util.Annotations as Annotations exposing (annotationToAttrAndLabel)
import Util.Data as Data
import Util.Graph exposing (decodeCoords, translate)
import Util.View exposing (onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (pickPathFunction)
import View.Pathfinder.Tx.Utils exposing (AnimatedPosTrait, signX, toPosition)


view : Plugins -> View.Config -> Pathfinder.Config -> Tx -> AccountTx -> Maybe Annotations.AnnotationItem -> Svg Msg
view _ vc _ tx accTx annotation =
    let
        fd =
            GraphComponents.txNodeEthNodeEllipse_details

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2

        offset =
            2
                + (if vc.showTimestampOnTxEdge then
                    3

                   else
                    -GraphComponents.txNodeEthTimestamp_details.height
                  )

        ( annAttr, label ) =
            annotation
                |> Maybe.map
                    (annotationToAttrAndLabel
                        tx
                        GraphComponents.txNodeEth_details
                        offset
                        UserOpensTxAnnotationDialog
                    )
                |> Maybe.withDefault ( [], [] )

        colorFinal =
            annotation
                |> Maybe.andThen .color
                |> Maybe.map Color.toCssString
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
                |> Maybe.withDefault Colors.pathMiddle

        ( dateLine, timeLine ) =
            let
                t =
                    Data.timestampToPosix accTx.raw.timestamp
            in
            if not vc.showHash then
                ( Locale.timestampDateUniform vc.locale t
                , Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset t
                )

            else
                ( Util.View.truncateLongIdentifier ("0x" ++ accTx.raw.txHash)
                , Locale.timestampDateTimeUniform vc.locale vc.showTimeZoneOffset t
                )
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
        (GraphComponents.txNodeEthWithAttributes
            { txNodeEthAttributes
                | root =
                    [ UserClickedTx tx.id |> onClickWithStop
                    , UserPushesLeftMouseButtonOnUtxoTx tx.id
                        |> Util.Graph.mousedown
                    , UserMovesMouseOverTx tx.id
                        |> onMouseOver
                    , UserMovesMouseOutTx tx.id
                        |> onMouseLeave
                    , css [ Css.cursor Css.pointer ]
                    , Id.toString tx.id
                        |> Svg.Styled.Attributes.id
                    , decodeCoords Coords.Coords
                        |> Json.Decode.map (\c -> ( UserOpensContextMenu c (ContextMenu.TransactionContextMenu tx.id), True ))
                        |> preventDefaultOn "contextmenu"
                    ]
                , nodeEllipse = annAttr
                , highlightEllipse = [ Css.property "stroke" colorFinal |> Css.important ] |> css |> List.singleton
            }
            { root =
                { highlightVisible = tx.selected || tx.hovered
                , date = dateLine
                , time =
                    if vc.showTimestampOnTxEdge then
                        timeLine

                    else
                        ""
                , inputValue = Locale.currency (View.toCurrency vc) vc.locale [ ( asset accTx.raw.network accTx.raw.currency, accTx.value ) ]
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


edge : Plugins -> View.Config -> Pathfinder.Config -> { t | hovered : Bool, conversionType : Maybe ConversionLegType } -> AccountTx -> AnimatedPosTrait x -> Maybe Annotations.AnnotationItem -> Svg Msg
edge _ _ _ { hovered, conversionType } tx aTxPos annotation =
    let
        radTx =
            GraphComponents.txNodeEthNodeEllipse_details.width / 2

        radA =
            GraphComponents.addressNodeNodeFrame_details.width / 2

        isConversionLeg =
            Maybe.Extra.isJust conversionType

        colorFinal =
            annotation
                |> Maybe.andThen .color
                |> Maybe.map Color.toCssString
                |> Maybe.Extra.or
                    (conversionType
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
    Maybe.map2
        (\fro too ->
            let
                txId =
                    Id.init tx.raw.network tx.raw.identifier

                txPos =
                    aTxPos |> toPosition

                fromPos =
                    fro |> toPosition

                toPos =
                    too |> toPosition

                leftSign =
                    signX fromPos txPos

                leftLeg =
                    ( Id.toString txId
                    , pickPathFunction False
                        hovered
                        colorFinal
                        ""
                        { x = fromPos.x * unit + (radA * leftSign), y = fromPos.y * unit }
                        { x = txPos.x * unit - (radTx * leftSign), y = txPos.y * unit }
                        (A.animate aTxPos.clock aTxPos.opacity)
                        isConversionLeg
                    )

                rightSign =
                    signX txPos toPos

                rightLeg =
                    ( Id.toString txId
                    , pickPathFunction True
                        hovered
                        colorFinal
                        ""
                        { x = txPos.x * unit + (radTx * rightSign), y = txPos.y * unit }
                        { x = toPos.x * unit - (radA * rightSign), y = toPos.y * unit }
                        (A.animate aTxPos.clock aTxPos.opacity)
                        isConversionLeg
                    )
            in
            [ leftLeg, rightLeg ]
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
        )
        tx.fromAddress
        tx.toAddress
        |> Maybe.withDefault (text "")
