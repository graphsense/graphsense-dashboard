module View.Pathfinder.Address exposing (toNodeIconHtml, view)

import Animation as A
import Api.Data
import Color exposing (Color)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Html.Styled
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address, Txs(..), expandAllowed, getTxs, txsGetSet)
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import RemoteData
import Svg.Styled exposing (Svg, g, image, text)
import Svg.Styled.Attributes as Svg exposing (css, opacity, transform)
import Svg.Styled.Events exposing (onMouseOver, stopPropagationOn)
import Theme.Html.GraphComponents as HtmlGraphComponents
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Util.Annotations as Annotations
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop, truncateLongIdentifierWithLengths)


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

        ( annAttr, label ) =
            case annotation of
                Just ann ->
                    let
                        colorAttributes prop =
                            case ann.color of
                                Just c ->
                                    Color.toCssString c
                                        |> Css.property prop
                                        |> Css.important
                                        |> List.singleton
                                        |> css
                                        |> List.singleton

                                _ ->
                                    []
                    in
                    ( colorAttributes "fill"
                    , (if String.length ann.label > 0 then
                        HtmlGraphComponents.annotationLabelWithAttributes
                            (HtmlGraphComponents.annotationLabelAttributes
                                |> Rs.s_annotationLabel
                                    (css
                                        [ Css.display Css.inlineBlock
                                        ]
                                        :: colorAttributes "border-color"
                                    )
                            )
                            { annotationLabel = { labelText = ann.label } }
                            |> List.singleton
                            |> Html.Styled.div
                                [ css
                                    [ Css.pct 100 |> Css.width
                                    , Css.textAlign Css.center
                                    ]
                                ]
                            |> List.singleton
                            |> Svg.Styled.foreignObject
                                [ translate
                                    0
                                    (GraphComponents.addressNode_details.height
                                        + (if address.exchange == Nothing then
                                            -GraphComponents.addressNodeExchangeLabel_details.height

                                           else
                                            0
                                          )
                                        + 2
                                    )
                                    |> transform
                                , GraphComponents.addressNode_details.width
                                    |> String.fromFloat
                                    |> Svg.width
                                , (GraphComponents.annotationLabel_details.height
                                    + GraphComponents.annotationLabel_details.strokeWidth
                                    * 2
                                  )
                                    * (1 + (toFloat <| String.length ann.label // 13))
                                    |> String.fromFloat
                                    |> Svg.height
                                , A.animate address.clock address.opacity
                                    |> String.fromFloat
                                    |> opacity
                                , UserOpensAddressAnnotationDialog address.id |> onClickWithStop
                                , css [ Css.cursor Css.pointer ]
                                ]

                       else
                        text ""
                      )
                        |> List.singleton
                    )

                _ ->
                    ( [], [] )
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
toNodeIcon highlight address _ clusterColor =
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
            Icons.iconsUntaggedWithAttributes (Icons.iconsUntaggedAttributes |> Rs.s_ellipse25 (getHighlight c)) {}

        ( Just _, Just c, _ ) ->
            let
                cattr =
                    getHighlight c
            in
            Icons.iconsExchangeWithAttributes (Icons.iconsExchangeAttributes |> Rs.s_dollar cattr |> Rs.s_arrows cattr) {}

        ( Just _, _, _ ) ->
            Icons.iconsExchange {}
