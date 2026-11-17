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
import View.Pathfinder.ConversionEdge as ConversionEdgeView
import View.Pathfinder.Details exposing (closeAttrs, dataTab, emptyCell, valuesToCell)
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
        (CTable.config tableStyles vc viewState.raw.raw.conversionType viewState.raw.id isTxOnGraph)
        viewState.table
    ]
        |> div []
        |> subTxsTab


{-| On the destination network: a bridge's output leg is not on the input's network.
-}
outputAsset : ConversionEdge -> AssetIdentifier
outputAsset c =
    case ConversionEdge.outputAmount c of
        ConversionEdge.Amount a _ ->
            a

        ConversionEdge.NotInRegistry ->
            asset c.raw.toNetwork c.raw.toAsset


{-| A leg's amount as today, or for a token outside the registry the marker in
its place: neither its raw base units nor a guessed scale could be told apart
from the bridged amount.
-}
amountCell : View.Config -> ConversionEdge.LegAmount -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
amountCell vc amount =
    case amount of
        ConversionEdge.Amount a values ->
            valuesToCell vc a values

        ConversionEdge.NotInRegistry ->
            { firstRowText = Locale.string vc.locale "Unsupported token"
            , secondRowText = ""
            , secondRowVisible = False
            }


view : View.Config -> ( Id, Id ) -> (Id -> Bool) -> ConversionDetailsModel -> Html Pathfinder.Msg
view vc _ isTxOnGraph viewState =
    let
        baseTxIdString =
            case viewState.raw.legTxs of
                Just { input } ->
                    case input of
                        Api.Data.TxTxAccount { txHash } ->
                            txHash

                        Api.Data.TxTxUtxo { txHash } ->
                            txHash

                Nothing ->
                    ConversionEdge.legTxHash cr.fromAssetTransfer

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

            -- a row names no time; the timestamp is the output leg tx's
            , block3 =
                if ConversionEdge.isRowOnly viewState.raw then
                    Just none

                else
                    Nothing
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
                    , if ConversionEdge.isRowOnly viewState.raw then
                        none

                      else
                        txTab vc isTxOnGraph viewState
                    ]
            , tabsVisible = False
            }
        , sidePanelSwapHeader = { headerText = title vc cr }
        , titleOfInputValue = { infoLabel = Locale.string vc.locale "Input Value" }
        , titleOfOutputValue = { infoLabel = Locale.string vc.locale "Output Value" }
        , titleOfReceiver = { infoLabel = Locale.string vc.locale "receiver" }
        , titleOfSender = { infoLabel = Locale.string vc.locale "sender" }
        , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
        , valueOfInputValue = ConversionEdge.inputAmount viewState.raw |> amountCell vc
        , valueOfOutputValue = ConversionEdge.outputAmount viewState.raw |> amountCell vc
        , valueOfReceiver = { copyIconInstance = copyIconPathfinderAbove vc cr.toAddress, firstRowText = cr.toAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfSender = { copyIconInstance = copyIconPathfinderAbove vc cr.fromAddress, firstRowText = cr.fromAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfTimestamp =
            viewState.raw.legTxs
                |> Maybe.map (.output >> getTimestamp >> timeToCell vc)
                |> Maybe.withDefault emptyCell
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
            , testId "gs-bridge-protocol"
            ]
            [ text sub ]
        ]


{-| What a bridge row adds below the timestamp: the protocol's own explorer,
laid out like the rows above it (label left, value right), from the same
generated styles. The legs are not repeated here: the Bridge transactions
section below lists both, copyable and linked to their chains' explorers.
Swaps, and protocols without an explorer (Symbiosis), add nothing.

A bridge drawn from its row (a token outside the registry) has no leg txs to
list. It names both tokens instead, as the edge does and with the full contract
to copy, and both txs, followed by why an amount is missing.

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

        explorerRows =
            protocolExplorer
                |> Maybe.map (\link -> [ row (Locale.string vc.locale "Bridge explorer") [ explorerAnchor link ] ])
                |> Maybe.withDefault []

        rowOnly =
            ConversionEdge.isRowOnly edge
    in
    case ( c.conversionType, explorerRows, rowOnly ) of
        ( Api.Data.ExternalConversionConversionTypeBridgeTx, [], False ) ->
            none

        ( Api.Data.ExternalConversionConversionTypeBridgeTx, _, _ ) ->
            div
                [ SidePanelComponents.sidePanelSwapTransactionSidePanelEthTxDetails_details.styles
                    ++ [ Css.paddingTop Css.zero ]
                    |> css
                ]
                (if rowOnly then
                    [ if List.isEmpty explorerRows then
                        none

                      else
                        block explorerRows
                    , block
                        [ tokenRow vc (Locale.string vc.locale "Input token") c.fromNetwork c.fromAsset c.fromIsSupportedAsset
                        , tokenRow vc (Locale.string vc.locale "Output token") c.toNetwork c.toAsset c.toIsSupportedAsset
                        , txRow vc (Locale.string vc.locale "Source tx") c.fromAssetTransfer
                        , txRow vc (Locale.string vc.locale "Destination tx") c.toAssetTransfer
                        ]
                    , unsupportedHint vc
                    ]

                 else
                    [ block explorerRows ]
                )

        _ ->
            none


{-| A token as the edge names it, the full contract to copy.
-}
tokenRow : View.Config -> String -> String -> String -> Bool -> Html msg
tokenRow vc label network asset supported =
    let
        copy =
            if asset == ConversionEdge.nativeAsset then
                []

            else
                [ copyIconPathfinder vc asset ]
    in
    rowWith [ testId "gs-bridge-token" ]
        label
        (truncatedValue (ConversionEdgeView.rowLegLabel vc.locale network asset supported) :: copy)


{-| A leg's tx, by its hash, to copy.
-}
txRow : View.Config -> String -> String -> Html msg
txRow vc label transfer =
    let
        hash =
            ConversionEdge.legTxHash transfer
    in
    row label
        [ truncatedValue (truncateLongIdentifierWithLengths 8 4 hash)
        , copyIconPathfinder vc hash
        ]


{-| One line, cut with an ellipsis: the value column has a fixed width.
-}
truncatedValue : String -> Html msg
truncatedValue value =
    span
        [ css
            [ Css.overflow Css.hidden
            , Css.textOverflow Css.ellipsis
            , Css.whiteSpace Css.noWrap
            , Css.minWidth Css.zero
            ]
        ]
        [ text value ]


{-| Below the rows, styled like the notes in a tx's panel. The panel is as
wide as its content, so a text of width 100% still widens it to the text's
single-line length; width 0 with min-width 100% fills the panel and wraps
within it instead.
-}
unsupportedHint : View.Config -> Html msg
unsupportedHint vc =
    div
        [ SidePanelComponents.sidePanelEthTransactionSidePanelEthTxDetailsText_details.styles
            ++ [ Css.paddingTop (Css.px 8)
               , Css.width Css.zero |> Css.important
               , Css.minWidth (Css.pct 100)
               ]
            |> css
        , testId "gs-bridge-unsupported-hint"
        ]
        [ text (Locale.string vc.locale "unsupported-bridge-token-hint") ]


block : List (Html msg) -> Html msg
block =
    div [ css SidePanelComponents.sidePanelSwapTransactionBlock2_details.styles ]


row : String -> List (Html msg) -> Html msg
row =
    rowWith []


rowWith : List (Html.Styled.Attribute msg) -> String -> List (Html msg) -> Html msg
rowWith attrs label value =
    div (css SidePanelComponents.sidePanelSwapTransactionSenderSender_details.styles :: attrs)
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
        , testId "gs-bridge-explorer-link"
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
