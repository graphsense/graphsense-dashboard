module View.Pathfinder.Address exposing (toNodeIcon, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Html.Styled.Attributes as Html
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
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop, truncateLongIdentifierWithLengths)


view : Plugins -> View.Config -> Pathfinder.Config -> Address -> Svg Msg
view _ vc _ address =
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
            case getTxs address direction of
                TxsLoading ->
                    []

                _ ->
                    [ UserClickedAddressExpandHandle address.id direction |> onClickWithStop
                    , Json.Decode.succeed ( NoOp, True )
                        |> stopPropagationOn "mousedown"
                    , css
                        [ Css.cursor Css.pointer
                        ]
                    ]

        fd =
            GraphComponents.addressNodeNodeFrameDetails

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2
    in
    GraphComponents.addressNodeWithInstances
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
        (GraphComponents.addressNodeInstances
            |> s_iconsNodeOpenLeft
                (GraphComponents.addressNodeIconsNodeOpenLeftDetails
                    |> expandHandleLoadingSpinner vc address Incoming
                )
            |> s_iconsNodeOpenRight
                (GraphComponents.addressNodeIconsNodeOpenRightDetails
                    |> expandHandleLoadingSpinner vc address Outgoing
                )
        )
        { addressNode =
            { addressId =
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


expandHandleLoadingSpinner : View.Config -> Address -> Direction -> { x : Float, y : Float, width : Float, height : Float, strokeWidth : Float, styles : List Css.Style } -> Maybe (Svg Msg)
expandHandleLoadingSpinner vc address direction details =
    if getTxs address direction == TxsLoading then
        let
            offset =
                5
        in
        image
            [ translate
                (details.x + offset / 2)
                (details.y + offset / 2)
                |> Svg.transform
            , details.width
                - offset
                |> String.fromFloat
                |> Svg.width
            , details.height
                - offset
                |> String.fromFloat
                |> Svg.height
            , Html.attribute "href" vc.theme.loadingSpinnerUrl
            ]
            []
            |> Just

    else
        Nothing


toNodeIcon : Address -> Svg msg
toNodeIcon address =
    if address.exchange == Nothing then
        Icons.iconsUntagged {}

    else
        Icons.iconsExchange {}
