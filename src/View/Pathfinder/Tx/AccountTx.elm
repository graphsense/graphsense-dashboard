module View.Pathfinder.Tx.AccountTx exposing (edge)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
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
import Theme.Svg.GraphComponents as GraphComponents
import Tuple exposing (pair)
import Util.Pathfinder exposing (getAddress)
import View.Locale as Locale
import View.Pathfinder.Tx.Path exposing (accountPath)



-- import View.Pathfinder.Tx.Utxo exposing (TxPos)
-- import Theme.Svg.GraphComponents as GraphComponents exposing (txNodeCircleAttributes)
-- import Util.Graph exposing (translate)
-- import Util.View exposing (onClickWithStop)
-- import Html.Styled.Events exposing (onMouseLeave)
-- import Css
-- view : Plugins -> View.Config -> Pathfinder.Config -> Id -> Bool -> AccountTx -> TxPos x -> Svg Msg
-- view _ vc pc id highlight tx pos =
--     let
--         fd =
--             GraphComponents.txNodeCircleTxNodeDetails
--         adjX =
--             fd.x + fd.width / 2
--         adjY =
--             fd.y + fd.height / 2
--     in
--     GraphComponents.txNodeCircleWithAttributes
--         { txNodeCircleAttributes
--             | txNodeCircle =
--                 [ translate
--                     ((pos.x + pos.dx) * unit - adjX)
--                     ((A.animate pos.clock pos.y + pos.dy) * unit - adjY)
--                     |> transform
--                 , A.animate pos.clock pos.opacity
--                     |> String.fromFloat
--                     |> opacity
--                 , UserClickedTx id |> onClickWithStop
--                 , UserPushesLeftMouseButtonOnUtxoTx id
--                     |> Util.Graph.mousedown
--                 , UserMovesMouseOverUtxoTx id
--                     |> onMouseOver
--                 , UserMovesMouseOutUtxoTx id
--                     |> onMouseLeave
--                 , css [ Css.cursor Css.pointer ]
--                 , Id.toString id
--                     |> Svg.Styled.Attributes.id
--                 ]
--         }
--         { txNodeCircle =
--             { hasMultipleInOutputs = False
--             , highlightVisible = highlight
--             , date = Locale.timestampDateUniform vc.locale tx.raw.timestamp
--             , time = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset tx.raw.timestamp
--             , timestampVisible = vc.showTimestampOnTxEdge
--             }
--         }


edge : Plugins -> View.Config -> Pathfinder.Config -> Dict Id Address -> AccountTx -> Svg Msg
edge _ vc _ addresses tx =
    let
        rad =
            GraphComponents.addressNodeNodeFrameDetails.width / 2
    in
    tx.to
        |> getAddress addresses
        |> Result.toMaybe
        |> Maybe.map2
            (\fro too ->
                let
                    label =
                        tx.value
                            |> pair { network = Id.network fro.id, asset = tx.raw.currency }
                            |> List.singleton
                            |> Locale.currency vc.locale
                in
                accountPath
                    vc
                    label
                    ((fro.x + fro.dx) * unit + rad)
                    ((A.animate fro.clock fro.y + fro.dy) * unit)
                    ((too.x + too.dx) * unit - rad)
                    ((A.animate too.clock too.y + too.dy) * unit)
                    (A.animate fro.clock fro.opacity
                        |> Basics.min (A.animate too.clock too.opacity)
                    )
            )
            (tx.from
                |> getAddress addresses
                |> Result.toMaybe
            )
        |> Maybe.withDefault (text "")
