module View.Pathfinder.Address exposing (toNodeIconHtml, view)

import Animation as A
import Color
import Config.Pathfinder as Pathfinder exposing (HideForExport(..), TracingMode(..))
import Config.View as View
import Css
import Dict
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onMouseLeave)
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address, AddressServiceType(..), Txs(..), expandAllowed, getTxs, isSmartContract, txsGetSet)
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Id as Id
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
import Util.Graph exposing (decodeCoords, translate)
import Util.View exposing (none, onClickWithStop, truncateLongIdentifierWithLengths)
import View.Locale as Locale


view : Plugins -> View.Config -> Pathfinder.Config -> Address -> Maybe Annotations.AnnotationItem -> Svg Msg
view plugins vc pc address annotation =
    let
        data =
            RemoteData.toMaybe address.data

        halfAlpha x =
            Color.fromRgba { red = x.red, green = x.green, blue = x.blue, alpha = x.alpha / 2 }

        clusterColorLight =
            address.clusterColor |> Maybe.map (Color.toRgba >> halfAlpha)

        clusterSiblingHovered =
            pc.highlightClusterFriends
                && address.clusterSiblingHovered

        highlightVisible =
            pc.hideForExport
                /= Exporting True
                && address.selected

        clusterStroke =
            case ( clusterColorLight, pc.highlightClusterFriends ) of
                ( Just color, True ) ->
                    [ css
                        [ Css.property "stroke" (Color.toCssString color) |> Css.important
                        , Css.property "stroke-width"
                            (if clusterSiblingHovered then
                                "5"

                             else
                                "3"
                            )
                            |> Css.important
                        ]
                    ]

                _ ->
                    []

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
                && (pc.tracingMode == TransactionTracingMode)
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
            address.actor
                |> Maybe.Extra.orElse address.exchange
                |> Maybe.Extra.orElse
                    (case address.addressServiceType of
                        LikelyUnknownService ->
                            if address |> isSmartContract then
                                Nothing

                            else
                                Just (Locale.string vc.locale "possible service")

                        _ ->
                            Nothing
                    )

        pluginTagIcons =
            Plugin.View.addressNodeTagIcon plugins address.plugins vc address

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
                    (annotationToAttrAndLabel vc
                        address
                        GraphComponents.addressNode_details
                        offset
                        UserOpensAddressAnnotationDialog
                    )
                |> Maybe.withDefault ( [], [] )

        ifTrue bool items =
            if bool then
                items

            else
                []

        icons =
            [ ifTrue address.hasTags [ Icons.iconsTagSwithoutPaddingTypeDirect {} ]
            , ifTrue (not address.hasTags && address.hasClusterTagsOnly) [ Icons.iconsTagSwithoutPaddingTypeIndirect {} ]
            , ifTrue (not <| List.isEmpty pluginTagIcons) pluginTagIcons
            , ifTrue (Dict.size address.networks > 1) [ Icons.iconsCrosschainSwithoutPadding {} ]
            ]
                |> List.concat

        iconInstance items index =
            List.Extra.getAt index items
                |> Maybe.withDefault none

        iconVisible items index =
            List.Extra.getAt index items
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False
    in
    g
        [ translate
            ((address.x + address.dx) * unit - adjX)
            ((A.animate address.clock address.y + address.dy) * unit - adjY)
            |> transform
        , address.selected
            |> Json.Encode.bool
            |> Json.Encode.encode 0
            |> Html.attribute "data-selected"
        ]
        (GraphComponents.addressNodeWithInstances
            (GraphComponents.addressNodeAttributes
                |> Rs.s_root
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
                |> Rs.s_clusterColor clusterStroke
             -- |> s_iconsStartingPoint [onMouseOver NoOp, onMouseLeave NoOp]
            )
            GraphComponents.addressNodeInstances
            { root =
                { addressId =
                    address.id
                        |> Id.id
                        |> truncateLongIdentifierWithLengths 8 4
                , highlightVisible = highlightVisible
                , clusterVisible = (address.clusterColor /= Nothing) && pc.highlightClusterFriends
                , expandLeftVisible = expandVisible Incoming
                , expandRightVisible = expandVisible Outgoing
                , mainIconInstance = toNodeIcon address
                , exchangeLabel = nodeLabel |> Maybe.withDefault ""
                , exchangeLabelVisible = nodeLabel /= Nothing
                , isStartingPoint = address.isStartingPoint || (pc.hideForExport /= Exporting True && address.selected)
                , icon1Visible = iconVisible icons 0
                , icon1Instance = iconInstance icons 0
                , icon2Visible = iconVisible icons 1
                , icon2Instance = iconInstance icons 1
                , icon3Visible = iconVisible icons 2
                , icon3Instance = iconInstance icons 2
                }
            , iconsNodeOpenLeft =
                { variant =
                    if pc.hideForExport /= NoExport then
                        none

                    else
                        expandHandleLoadingSpinner vc address Incoming Icons.iconsNodeOpenLeftStateActiv_details
                            |> Maybe.withDefault
                                (Icons.iconsNodeOpenLeftWithAttributes
                                    (Icons.iconsNodeOpenLeftAttributes
                                        |> Rs.s_root (expand Incoming)
                                    )
                                    { root =
                                        { state =
                                            if expandAllowed address then
                                                Icons.IconsNodeOpenLeftStateActiv

                                            else
                                                Icons.IconsNodeOpenLeftStateDisabled
                                        }
                                    }
                                )
                }
            , iconsNodeOpenRight =
                { variant =
                    if pc.hideForExport /= NoExport then
                        none

                    else
                        expandHandleLoadingSpinner vc address Outgoing Icons.iconsNodeOpenRightStateActiv_details
                            |> Maybe.withDefault
                                (Icons.iconsNodeOpenRightWithAttributes
                                    (Icons.iconsNodeOpenRightAttributes
                                        |> Rs.s_root (expand Outgoing)
                                    )
                                    { root =
                                        { state =
                                            if expandAllowed address then
                                                Icons.IconsNodeOpenRightStateActiv

                                            else
                                                Icons.IconsNodeOpenRightStateDisabled
                                        }
                                    }
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


toNodeIconHtml : Address -> Svg msg
toNodeIconHtml address =
    toNodeIcon address
        |> List.singleton
        |> Svg.svg
            [ Svg.width "24"
            , Svg.height "24"
            ]


toNodeIcon : Address -> Svg msg
toNodeIcon address =
    case ( address.exchange, address.data |> RemoteData.toMaybe |> Maybe.andThen .isContract ) of
        ( Just _, _ ) ->
            Icons.iconsExchangeL {}

        ( Nothing, Just True ) ->
            Icons.iconsSmartContractL {}

        ( Nothing, _ ) ->
            case address.addressServiceType of
                KnownService ->
                    Icons.iconsInstitutionL {}

                LikelyUnknownService ->
                    Icons.iconsUnknownServiceL {}

                UnknownService ->
                    Icons.iconsUntagged {}
