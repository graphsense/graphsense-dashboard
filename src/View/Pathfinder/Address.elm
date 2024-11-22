module View.Pathfinder.Address exposing (toNodeIconHtml, view)

import Animation as A
import Api.Data
import Color
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Json.Decode
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address, Txs(..), expandAllowed, getTxs, txsGetSet)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import RemoteData
import Svg.Styled as Svg exposing (Svg, g, image, text)
import Svg.Styled.Attributes as Svg exposing (css, opacity, transform)
import Svg.Styled.Events exposing (onMouseOver, preventDefaultOn, stopPropagationOn)
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Util.Annotations as Annotations exposing (annotationToAttrAndLabel)
import Util.Data exposing (isAccountLike)
import Util.Graph exposing (decodeCoords, translate)
import Util.View exposing (onClickWithStop, truncateLongIdentifierWithLengths)
import View.Locale as Locale


view : Plugins -> View.Config -> Pathfinder.Config -> Colors.ScopedColorAssignment -> Address -> (Id -> Maybe Api.Data.Entity) -> Maybe Annotations.AnnotationItem -> Svg Msg
view _ vc _ colors address getCluster annotation =
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

        nodeLabel =
            address.exchange
                |> Maybe.Extra.or address.actor
                |> Maybe.Extra.or
                    (case getAddressType address cluster of
                        LikelyUnknownService ->
                            Just (Locale.string vc.locale "possible service")

                        _ ->
                            Nothing
                    )

        offset =
            2
                + (if nodeLabel == Nothing then
                    -GraphComponents.addressNodeExchangeLabel_details.height

                   else
                    0
                  )

        ( annAttr, label ) =
            annotation
                |> Maybe.map
                    (annotationToAttrAndLabel
                        address
                        GraphComponents.addressNode_details
                        offset
                    )
                |> Maybe.withDefault ( [], [] )
    in
    g
        [ translate
            ((address.x + address.dx) * unit - adjX)
            ((A.animate address.clock address.y + address.dy) * unit - adjY)
            |> transform
        ]
        (GraphComponents.addressNodeWithAttributes
            (GraphComponents.addressNodeAttributes
                |> Rs.s_addressNode
                    [ A.animate address.clock address.opacity
                        |> String.fromFloat
                        |> opacity
                    , UserClickedAddress address.id |> onClickWithStop
                    , UserPushesLeftMouseButtonOnAddress address.id
                        |> Util.Graph.mousedown
                    , decodeCoords Coords.Coords
                        |> Json.Decode.map (\c -> ( UserOpensContextMenu c (ContextMenu.AddressContextMenu address.id), True ))
                        |> preventDefaultOn "contextmenu"
                    , css [ Css.cursor Css.pointer ]
                    ]
                |> Rs.s_nodeBody
                    [ Id.toString address.id
                        |> Svg.id
                    , UserMovesMouseOverAddress address.id
                        |> onMouseOver
                    , UserMovesMouseOutAddress address.id
                        |> onMouseLeave
                    ]
                |> Rs.s_nodeFrame annAttr
                |> Rs.s_clusterColor
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
                , iconInstance = toNodeIcon address cluster
                , exchangeLabel = nodeLabel |> Maybe.withDefault ""
                , exchangeLabelVisible = nodeLabel /= Nothing
                , isStartingPoint = address.isStartingPoint || address.selected
                , tagIconVisible = address.hasTags
                }
            , iconsNodeOpenLeft =
                { variant =
                    expandHandleLoadingSpinner vc address Incoming Icons.iconsNodeOpenLeftStateActiv_details
                        |> Maybe.withDefault
                            (if expandAllowed address then
                                Icons.iconsNodeOpenLeftStateActivWithAttributes
                                    (Icons.iconsNodeOpenLeftStateActivAttributes
                                        |> Rs.s_stateActiv (expand Incoming)
                                    )
                                    {}

                             else
                                Icons.iconsNodeOpenLeftStateDisabledWithAttributes
                                    (Icons.iconsNodeOpenRightStateDisabledAttributes
                                        |> Rs.s_stateDisabled (expand Incoming)
                                    )
                                    {}
                            )
                }
            , iconsNodeOpenRight =
                { variant =
                    expandHandleLoadingSpinner vc address Outgoing Icons.iconsNodeOpenRightStateActiv_details
                        |> Maybe.withDefault
                            (if expandAllowed address then
                                Icons.iconsNodeOpenRightStateActivWithAttributes
                                    (Icons.iconsNodeOpenRightStateActivAttributes
                                        |> Rs.s_stateActiv (expand Outgoing)
                                    )
                                    {}

                             else
                                Icons.iconsNodeOpenRightStateDisabledWithAttributes
                                    (Icons.iconsNodeOpenRightStateDisabledAttributes
                                        |> Rs.s_stateDisabled (expand Outgoing)
                                    )
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
            :: label
        )


expandHandleLoadingSpinner : View.Config -> Address -> Direction -> { x : Float, y : Float, width : Float, height : Float, renderedWidth : Float, renderedHeight : Float, strokeWidth : Float, styles : List Css.Style } -> Maybe (Svg Msg)
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


toNodeIconHtml : Maybe Address -> Maybe Api.Data.Entity -> Svg msg
toNodeIconHtml address cluster =
    (case address of
        Just addr ->
            toNodeIcon addr cluster

        Nothing ->
            Icons.iconsUntagged {}
    )
        |> List.singleton
        |> Svg.svg
            [ Svg.width "24"
            , Svg.height "24"
            ]


type AddressServiceType
    = KnownService
    | LikelyUnknownService
    | Unknown


getAddressType : Address -> Maybe Api.Data.Entity -> AddressServiceType
getAddressType address cluster =
    let
        clstrSize =
            cluster |> Maybe.map .noAddresses |> Maybe.withDefault 0

        clusterIndegree =
            cluster |> Maybe.map .inDegree |> Maybe.withDefault 0

        maxClusterSizeUser =
            100

        maxInDegreeUser =
            7500
    in
    if clstrSize > maxClusterSizeUser || clusterIndegree > maxInDegreeUser then
        if address.actor == Nothing then
            LikelyUnknownService

        else
            KnownService

    else if (address.id |> Id.network |> isAccountLike) && (address.actor |> Maybe.Extra.isJust) then
        KnownService

    else
        Unknown


toNodeIcon : Address -> Maybe Api.Data.Entity -> Svg msg
toNodeIcon address cluster =
    case ( address.exchange, address.data |> RemoteData.toMaybe |> Maybe.andThen .isContract ) of
        ( Just _, _ ) ->
            Icons.iconsExchange {}

        ( Nothing, Just True ) ->
            Icons.iconsSmartContract {}

        ( Nothing, _ ) ->
            case getAddressType address cluster of
                KnownService ->
                    Icons.iconsInstitution {}

                LikelyUnknownService ->
                    Icons.iconsUnknownService {}

                Unknown ->
                    Icons.iconsUntagged {}
