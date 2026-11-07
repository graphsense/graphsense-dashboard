module View.Pathfinder.ConversionDetails exposing (bridgeRows, outputAsset, subtitle, title, view)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Pathfinder exposing (sidePanelCss)
import Css.Table
import Html.Styled exposing (Html, div, span, text)
import Html.Styled.Attributes exposing (href, rel, target)
import Model.Currency exposing (AssetIdentifier, asset)
import Model.Pathfinder.ConversionDetails exposing (ConversionDetailsModel)
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder as Pathfinder
import Msg.Pathfinder.ConversionDetails exposing (ConversionDetailsMsgs(..))
import RecordSetter as Rs
import Svg.Styled.Attributes exposing (css)
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.BridgeExplorer as BridgeExplorer exposing (ExplorerLink)
import Util.Css exposing (spread)
import Util.View exposing (copyIconPathfinder, copyIconPathfinderAbove, none, testId, timeToCell, truncateLongIdentifierWithLengths)
import View.Graph.Table
import View.Locale as Locale
import View.Pathfinder.Details exposing (closeAttrs, dataTab, valuesToCell)
import View.Pathfinder.Table.ConversionTransactionTable as CTable


txTab : View.Config -> (Id -> Bool) -> ConversionDetailsModel -> Html Pathfinder.Msg
txTab vc isTxOnGraph viewState =
    let
        tableStyles =
            Css.Table.styles
                |> Rs.s_root
                    (Css.Table.styles.root
                        >> flip (++)
                            [ Css.display Css.block
                            , Css.width (Css.pct 100)
                            ]
                    )

        subTxsTab c =
            dataTab
                { title =
                    SidePanelComponents.sidePanelListHeaderTitleWithAttributes
                        (SidePanelComponents.sidePanelListHeaderTitleAttributes
                            |> Rs.s_root [ spread ]
                        )
                        { root =
                            { label =
                                case viewState.raw.raw.conversionType of
                                    Api.Data.ExternalConversionConversionTypeDexSwap ->
                                        Locale.string vc.locale "Swap transactions"

                                    Api.Data.ExternalConversionConversionTypeBridgeTx ->
                                        Locale.string vc.locale "Bridge transactions"
                            }
                        }
                , disabled = False
                , content =
                    if viewState.isConversionLegTableOpen then
                        Just c

                    else
                        Nothing
                , onClick =
                    UserTogglesConversionLegTable
                        |> Pathfinder.ConversionDetailsMsg viewState.raw.id
                }
    in
    [ View.Graph.Table.table
        tableStyles
        vc
        []
        (CTable.config tableStyles vc viewState.raw.id isTxOnGraph)
        viewState.table
    ]
        |> div []
        |> subTxsTab


inputAsset : ConversionEdge -> AssetIdentifier
inputAsset c =
    asset c.raw.fromNetwork (c.rawInputTransaction |> Tx.getAssetFromRawTx)


{-| On the destination network: a bridge's output leg is not on the input's network.
-}
outputAsset : ConversionEdge -> AssetIdentifier
outputAsset c =
    asset c.raw.toNetwork (c.rawOutputTransaction |> Tx.getAssetFromRawTx)


view : View.Config -> ( Id, Id ) -> (Id -> Bool) -> ConversionDetailsModel -> Html Pathfinder.Msg
view vc _ isTxOnGraph viewState =
    let
        baseTxIdString =
            case viewState.raw.rawInputTransaction of
                Api.Data.TxTxAccount { txHash } ->
                    txHash

                Api.Data.TxTxUtxo { txHash } ->
                    txHash

        getTimestamp x =
            case x of
                Api.Data.TxTxAccount { timestamp } ->
                    timestamp

                Api.Data.TxTxUtxo { timestamp } ->
                    timestamp

        cr =
            viewState.raw.raw

        instances =
            SidePanelComponents.sidePanelSwapTransactionInstances
    in
    SidePanelComponents.sidePanelSwapTransactionWithInstances
        (SidePanelComponents.sidePanelSwapTransactionAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            -- a two-line title keeps the close icon level with its first line
            |> Rs.s_titleEsc [ css [ Css.alignItems Css.flexStart ] ]
            |> Rs.s_iconsCloseBlack (closeAttrs Pathfinder.UserClosedDetailsView)
        )
        { instances
            | swapTransaction =
                subtitle cr |> Maybe.map (titleWithSubtitle (title vc cr))
        }
        { identifierWithCopyIcon =
            { identifier = baseTxIdString |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = baseTxIdString |> copyIconPathfinder vc
            , chevronInstance = none
            , addTagIconInstance = none
            }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , root =
            { subTxListInstance =
                div []
                    [ bridgeRows vc viewState.raw
                    , txTab vc isTxOnGraph viewState
                    ]
            , tabsVisible = False
            }
        , sidePanelSwapHeader = { headerText = title vc cr }
        , titleOfInputValue = { infoLabel = Locale.string vc.locale "Input Value" }
        , titleOfOutputValue = { infoLabel = Locale.string vc.locale "Output Value" }
        , titleOfReceiver = { infoLabel = Locale.string vc.locale "receiver" }
        , titleOfSender = { infoLabel = Locale.string vc.locale "sender" }
        , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
        , valueOfInputValue =
            ConversionEdge.inputValues viewState.raw
                |> valuesToCell vc (inputAsset viewState.raw)
        , valueOfOutputValue =
            ConversionEdge.outputValues viewState.raw
                |> valuesToCell vc (outputAsset viewState.raw)
        , valueOfReceiver = { copyIconInstance = copyIconPathfinderAbove vc cr.toAddress, firstRowText = cr.toAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfSender = { copyIconInstance = copyIconPathfinderAbove vc cr.fromAddress, firstRowText = cr.fromAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfTimestamp = viewState.raw.rawOutputTransaction |> getTimestamp |> timeToCell vc
        }


title : View.Config -> Api.Data.ExternalConversion -> String
title vc c =
    case c.conversionType of
        Api.Data.ExternalConversionConversionTypeDexSwap ->
            Locale.string vc.locale "Swap transaction"

        Api.Data.ExternalConversionConversionTypeBridgeTx ->
            Locale.string vc.locale "Bridge transaction"


{-| The protocol the API says proves a bridge row, and the app on top of it
when the API names one ("LayerZero · USDT0"), shown on a line of its own below
the title: the title box has a fixed width, and a protocol name beside the
title would crowd it.
-}
subtitle : Api.Data.ExternalConversion -> Maybe String
subtitle c =
    case c.conversionType of
        Api.Data.ExternalConversionConversionTypeDexSwap ->
            Nothing

        Api.Data.ExternalConversionConversionTypeBridgeTx ->
            c.protocol
                |> Maybe.map
                    (\protocol ->
                        case c.protocolApp of
                            Just app ->
                                BridgeExplorer.protocolName protocol ++ " · " ++ BridgeExplorer.appName app

                            Nothing ->
                                BridgeExplorer.protocolName protocol
                    )


{-| The generated title, with the protocol below it in the timestamp's second
row colour, cut with an ellipsis rather than widening the panel.
-}
titleWithSubtitle : String -> String -> Html msg
titleWithSubtitle main sub =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "2px"
            , Css.minWidth Css.zero
            ]
        ]
        [ div [ css SidePanelComponents.sidePanelSwapTransactionSidePanelSwapHeaderSwapTransaction_details.styles ]
            [ text main ]
        , div
            [ SidePanelComponents.sidePanelSwapTransactionSidePanelSwapHeaderExchangeLabel_details.styles
                ++ [ Css.property "color" "var(--c-greyBlue500)"
                   , Css.display Css.block
                   , Css.overflow Css.hidden
                   , Css.textOverflow Css.ellipsis
                   , Css.maxWidth (Css.pct 100)
                   ]
                |> css
            , testId "bridge-protocol"
            ]
            [ text sub ]
        ]


{-| What a bridge row adds below the timestamp: the protocol's own explorer,
laid out like the rows above it (label left, value right), from the same
generated styles. The legs are not repeated here: the Bridge transactions
section below lists both, copyable and linked to their chains' explorers.
Swaps, and protocols without an explorer (CCTP), add nothing.
-}
bridgeRows : View.Config -> ConversionEdge -> Html msg
bridgeRows vc edge =
    let
        c =
            edge.raw

        protocolExplorer =
            c.protocol
                |> Maybe.andThen
                    (\protocol ->
                        BridgeExplorer.explorerLink
                            { protocol = protocol
                            , sourceLeg = c.fromAssetTransfer
                            , transferId = c.transferId
                            }
                    )
    in
    case ( c.conversionType, protocolExplorer ) of
        ( Api.Data.ExternalConversionConversionTypeBridgeTx, Just link ) ->
            div
                [ SidePanelComponents.sidePanelSwapTransactionSidePanelEthTxDetails_details.styles
                    ++ [ Css.paddingTop Css.zero ]
                    |> css
                ]
                [ block [ row (Locale.string vc.locale "Bridge explorer") [ explorerAnchor link ] ] ]

        _ ->
            none


block : List (Html msg) -> Html msg
block =
    div [ css SidePanelComponents.sidePanelSwapTransactionBlock2_details.styles ]


row : String -> List (Html msg) -> Html msg
row label value =
    div [ css SidePanelComponents.sidePanelSwapTransactionSenderSender_details.styles ]
        [ div [ css SidePanelComponents.sidePanelSwapTransactionSenderTitleOfSender_details.styles ]
            [ div [ css SidePanelComponents.sidePanelSwapTransactionSenderLastUsageOfSender_details.styles ]
                [ text label ]
            ]
        , div [ css SidePanelComponents.sidePanelSwapTransactionSenderValueOfSender_details.styles ] value
        ]


{-| An explorer opens in a new tab, only on click, and is not told where the
click came from.
-}
explorerAnchor : ExplorerLink -> Html msg
explorerAnchor link =
    Html.Styled.a
        [ href link.url
        , target "_blank"
        , rel "noopener noreferrer"
        , testId "bridge-explorer-link"
        , css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "gap" "3px"
            , Css.marginLeft (Css.px 6)
            , Css.color Css.inherit
            , Css.textDecoration Css.none
            , Css.hover [ Css.textDecoration Css.underline ]
            ]
        ]
        [ span [ css SidePanelComponents.sidePanelSwapTransactionSenderAug052024OfSender_details.styles ] [ text link.label ]
        , HIcons.iconsGoToSnoPadding {}
        ]
