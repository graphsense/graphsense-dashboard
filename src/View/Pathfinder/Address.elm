module View.Pathfinder.Address exposing (toNodeIcon, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Html.Styled.Events exposing (onMouseLeave)
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (..)
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData
import Set
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop, truncateLongIdentifierWithLengths)


view : Plugins -> View.Config -> Pathfinder.Config -> Address -> Svg Msg
view _ _ _ address =
    let
        data =
            RemoteData.toMaybe address.data

        directionToField direction =
            case direction of
                Incoming ->
                    .noIncomingTxs

                Outgoing ->
                    .noOutgoingTxs

        nonZero direction =
            data
                |> Maybe.map (directionToField direction)
                |> Maybe.map ((<) 0)
                |> Maybe.withDefault False

        expandVisible direction =
            nonZero direction
                && (getTxs address direction
                        |> txsGetSet
                        |> (==) Nothing
                   )
                && (address.exchange == Nothing)

        expand direction =
            [ UserClickedAddressExpandHandle address.id direction |> onClickWithStop
            , Json.Decode.succeed ( NoOp, True )
                |> stopPropagationOn "mousedown"
            , css
                [ Css.cursor Css.pointer
                , Css.opacity <|
                    Css.num <|
                        if getTxs address direction == TxsLoading then
                            0.5

                        else
                            1
                ]
            ]

        fd =
            GraphComponents.addressNodeNodeFrameDetails

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2
    in
    GraphComponents.addressNodeWithAttributes
        (GraphComponents.addressNodeAttributes
            |> s_addressNode
                [ translate
                    ((address.x + address.dx) * unit - adjX)
                    ((A.animate address.clock address.y + address.dy) * unit - adjY)
                    |> transform
                , A.animate address.clock address.opacity
                    |> String.fromFloat
                    |> opacity
                , UserClickedAddress address.id |> onClickWithStop
                , UserPushesLeftMouseButtonOnAddress address.id
                    |> Util.Graph.mousedown
                , UserMovesMouseOverAddress address.id
                    |> onMouseOver
                , UserMovesMouseOutAddress address.id
                    |> onMouseLeave
                , css [ Css.cursor Css.pointer ]
                ]
            |> s_nodeFrame
                [ Id.toString address.id
                    |> Svg.id
                ]
            |> s_iconsNodeOpenLeft (expand Incoming)
            |> s_iconsNodeOpenRight (expand Outgoing)
        )
        { addressNode =
            { label =
                address.id
                    |> Id.id
                    |> truncateLongIdentifierWithLengths 8 4
            , highlightVisible = address.selected
            , expandLeftVisible = expandVisible Incoming
            , expandRightVisible = expandVisible Outgoing
            , iconInstance = toNodeIcon address
            , exchangeLabel =
                address.exchange
                    |> Maybe.withDefault ""
            , exchangeLabelVisible = address.exchange /= Nothing
            , isStartingPoint = address.isStartingPoint
            , tagIconVisible = address.hasTags
            }
        }


toNodeIcon : Address -> Svg msg
toNodeIcon address =
    if address.exchange == Nothing then
        Icons.iconsUntagged {}

    else
        Icons.iconsExchange {}
