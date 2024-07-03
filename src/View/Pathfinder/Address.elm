module View.Pathfinder.Address exposing (toNodeIcon, view)

import Animation as A
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
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

        nonZero field =
            data
                |> Maybe.map field
                |> Maybe.map ((<) 0)
                |> Maybe.withDefault False

        plus direction =
            [ UserClickedAddressExpandHandle address.id direction |> onClickWithStop
            , Json.Decode.succeed ( NoOp, True )
                |> stopPropagationOn "mousedown"
            , css [ Css.cursor Css.pointer ]
            ]

        fd =
            GraphComponents.addressNodeNodeFrameDimensions

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2
    in
    GraphComponents.addressNode
        (GraphComponents.defaultAddressNodeAttributes
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
                , css [ Css.cursor Css.pointer ]
                ]
            |> s_iconsPlusIn (plus Incoming)
            |> s_iconsPlusOut (plus Outgoing)
        )
        { addressId =
            address.id
                |> Id.id
                |> truncateLongIdentifierWithLengths 6 3
        , highlight = address.selected
        , plusInVisible = nonZero .noIncomingTxs && Set.isEmpty address.incomingTxs
        , plusOutVisible = nonZero .noOutgoingTxs && Set.isEmpty address.outgoingTxs
        , nodeIcon = toNodeIcon address
        , exchangeLabel =
            address.exchange
                |> Maybe.withDefault ""
        , startingPoint = address.isStartingPoint
        , tagIcon = address.hasTags
        }


toNodeIcon : Address -> Svg msg
toNodeIcon address =
    if address.exchange == Nothing then
        Icons.iconsUntagged Icons.defaultIconsUntaggedAttributes {}

    else
        Icons.iconsExchange Icons.defaultIconsExchangeAttributes {}
