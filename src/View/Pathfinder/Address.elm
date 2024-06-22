module View.Pathfinder.Address exposing (view)

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
import Theme.PathfinderComponents as PathfinderComponents
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop, truncateLongIdentifier)


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
            [ UserClickedAddressExpandHandle address.id direction |> onClick
            , Json.Decode.succeed ( NoOp, True )
                |> stopPropagationOn "mousedown"
            , css [ Css.cursor Css.pointer ]
            ]

        fd =
            PathfinderComponents.addressNodeFrameDimensions

        adjX =
            fd.x + fd.width / 2

        adjY =
            fd.y + fd.height / 2
    in
    PathfinderComponents.addressNode
        (PathfinderComponents.defaultAddressNodeAttributes
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
            |> s_plusIn (plus Incoming)
            |> s_plusOut (plus Outgoing)
        )
        { label =
            address.id
                |> Id.id
                |> truncateLongIdentifier
        , highlight = address.selected
        , plusInVisible = nonZero .noIncomingTxs && Set.isEmpty address.incomingTxs
        , plusOutVisible = nonZero .noOutgoingTxs && Set.isEmpty address.outgoingTxs
        , nodeIcon = PathfinderComponents.normalNodeIcon PathfinderComponents.defaultNormalNodeIconAttributes {}
        }
