module View.Pathfinder.Tx.AccountTx exposing (edge, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Dict exposing (Dict)
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (unit)
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
import Theme.Svg.GraphComponents as GraphComponents exposing (txNodeCircleAttributes)
import Util.Graph exposing (translate)
import Util.Pathfinder exposing (getAddress)
import Util.View exposing (onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (inPath, inPathHovered, outPath, outPathHovered)
import View.Pathfinder.Tx.Utils exposing (AnimatedPosTrait, signX, toPosition)


view : Plugins -> View.Config -> Pathfinder.Config -> Id -> Bool -> AccountTx -> AnimatedPosTrait x -> Svg Msg
view _ vc pc id highlight tx pos =
    let
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
                    ((pos.x + pos.dx) * unit - adjX)
                    ((A.animate pos.clock pos.y + pos.dy) * unit - adjY)
                    |> transform
                , A.animate pos.clock pos.opacity
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
            { hasMultipleInOutputs = False
            , highlightVisible = highlight
            , date = Locale.timestampDateUniform vc.locale tx.raw.timestamp
            , time = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset tx.raw.timestamp
            , timestampVisible = vc.showTimestampOnTxEdge
            }
        }


edge : Plugins -> View.Config -> Pathfinder.Config -> Bool -> Dict Id Address -> AccountTx -> AnimatedPosTrait x -> Svg Msg
edge _ vc _ hovered addresses tx aTxPos =
    let
        radTx =
            GraphComponents.txNodeCircleTxNodeDetails.width / 2

        radA =
            GraphComponents.addressNodeNodeFrameDetails.width / 2
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
                    , Svg.lazy7
                        (if hovered then
                            inPathHovered

                         else
                            inPath
                        )
                        vc
                        ""
                        (fromPos.x * unit + (radA * leftSign))
                        (fromPos.y * unit)
                        (txPos.x * unit - (radTx * leftSign))
                        (txPos.y * unit)
                        (A.animate aTxPos.clock aTxPos.opacity)
                    )

                rightSign =
                    signX txPos toPos

                rightLeg =
                    ( Id.toString txId
                    , Svg.lazy7
                        (if hovered then
                            outPathHovered

                         else
                            outPath
                        )
                        vc
                        ""
                        (txPos.x * unit + (radTx * rightSign))
                        (txPos.y * unit)
                        (toPos.x * unit - (radA * rightSign))
                        (toPos.y * unit)
                        (A.animate aTxPos.clock aTxPos.opacity)
                    )
            in
            [ leftLeg, rightLeg ]
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
        )
        (tx.from
            |> getAddress addresses
            |> Result.toMaybe
        )
        (tx.to
            |> getAddress addresses
            |> Result.toMaybe
        )
        |> Maybe.withDefault (text "")
