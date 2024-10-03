module View.Pathfinder.Address exposing (toNodeIconHtml, view)

import Animation as A
import Api.Data
import Color exposing (Color)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (..)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events exposing (..)
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop, truncateLongIdentifierWithLengths)


view : Plugins -> View.Config -> Pathfinder.Config -> Colors.ScopedColorAssignment -> Address -> (Id -> Maybe Api.Data.Entity) -> Svg Msg
view _ vc _ colors address getCluster =
    let
        data =
            RemoteData.toMaybe address.data

        clusterid =
            data |> Maybe.map (\z -> Id.initClusterId z.currency z.entity)

        clusterColor =
            clusterid |> Maybe.andThen (\x -> Colors.getAssignedColor Colors.Clusters x colors) |> Maybe.map .color

        halfAlpha x =
            Color.fromRgba { red = x.red, green = x.green, blue = x.blue, alpha = x.alpha / 2 }

        clusterColorLight =
            clusterColor |> Maybe.map (Color.toRgba >> halfAlpha)

        cluster =
            clusterid |> Maybe.andThen getCluster

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
            GraphComponents.addressNodeNodeFrame_details

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
                , css [ Css.cursor Css.pointer ]
                ]
            |> s_nodeBody
                [ Id.toString address.id
                    |> Svg.id
                , UserMovesMouseOverAddress address.id
                    |> onMouseOver
                , UserMovesMouseOutAddress address.id
                    |> onMouseLeave
                ]
            |> s_clusterColor
                (case ( clusterColorLight, vc.highlightClusterFriends ) of
                    ( Just c, True ) ->
                        [ css [ Css.property "stroke" (Color.toCssString c) |> Css.important ] ]

                    _ ->
                        []
                )
         -- |> s_iconsStartingPoint [onMouseOver NoOp, onMouseLeave NoOp]
        )
        { addressNode =
            { addressId =
                address.id
                    |> Id.id
                    |> truncateLongIdentifierWithLengths 8 4
            , highlightVisible = address.selected
            , clusterVisible = (clusterColor /= Nothing) && vc.highlightClusterFriends
            , expandLeftVisible = expandVisible Incoming
            , expandRightVisible = expandVisible Outgoing
            , iconInstance = toNodeIcon vc.highlightClusterFriends address cluster Nothing
            , exchangeLabel =
                address.exchange
                    |> Maybe.withDefault ""
            , exchangeLabelVisible = address.exchange /= Nothing
            , isStartingPoint = address.isStartingPoint || address.selected
            , tagIconVisible = address.hasTags
            }
        , iconsNodeOpenLeft =
            { variant =
                expandHandleLoadingSpinner vc address Incoming Icons.iconsNodeOpenLeftStateActiv_details
                    |> Maybe.withDefault
                        (Icons.iconsNodeOpenLeftStateActivWithAttributes
                            (Icons.iconsNodeOpenLeftStateActivAttributes |> s_stateActiv (expand Incoming))
                            {}
                        )
            }
        , iconsNodeOpenRight =
            { variant =
                expandHandleLoadingSpinner vc address Outgoing Icons.iconsNodeOpenRightStateActiv_details
                    |> Maybe.withDefault
                        (Icons.iconsNodeOpenRightStateActivWithAttributes
                            (Icons.iconsNodeOpenRightStateActivAttributes |> s_stateActiv (expand Outgoing))
                            {}
                        )
            }
        , iconsNodeMarker =
            { variant =
                case ( address.selected, address.isStartingPoint ) of
                    ( True, _ ) ->
                        Icons.iconsNodeMarkerPurposeSelectedNode {}

                    ( False, False ) ->
                        text ""

                    ( False, True ) ->
                        Icons.iconsNodeMarkerPurposeStartingPoint {}
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


toNodeIconHtml : Bool -> Maybe Address -> Maybe Api.Data.Entity -> Maybe Color -> Svg msg
toNodeIconHtml highlight address cluster clusterColor =
    (case address of
        Just addr ->
            toNodeIcon highlight addr cluster clusterColor

        Nothing ->
            Icons.iconsUntagged {}
    )
        |> List.singleton
        |> Svg.Styled.svg
            [ Svg.width "24"
            , Svg.height "24"
            ]


toNodeIcon : Bool -> Address -> Maybe Api.Data.Entity -> Maybe Color -> Svg msg
toNodeIcon highlight address cluster clusterColor =
    let
        -- clstrSize =
        --     cluster |> Maybe.map .noAddresses |> Maybe.withDefault 0
        getHighlight c =
            if highlight then
                [ css ((Util.View.toCssColor >> Css.fill >> Css.important >> List.singleton) c) ]

            else
                []
    in
    case ( address.exchange, clusterColor, address.data |> RemoteData.toMaybe |> Maybe.andThen .isContract ) of
        ( _, _, Just True ) ->
            Icons.iconsSmartContract {}

        ( Nothing, Nothing, _ ) ->
            {-
               if clstrSize > 1 then
                   Icons.iconsCluster {}

               else
            -}
            Icons.iconsUntagged {}

        ( Nothing, Just c, _ ) ->
            {- if clstrSize > 1 then
                   Icons.iconsClusterWithAttributes (Icons.iconsClusterAttributes |> s_vector (getHighlight c)) {}

               else
            -}
            Icons.iconsUntaggedWithAttributes (Icons.iconsUntaggedAttributes |> s_ellipse25 (getHighlight c)) {}

        ( Just _, Just c, _ ) ->
            let
                cattr =
                    getHighlight c
            in
            Icons.iconsExchangeWithAttributes (Icons.iconsExchangeAttributes |> s_dollar cattr |> s_arrows cattr) {}

        ( Just _, _, _ ) ->
            Icons.iconsExchange {}
