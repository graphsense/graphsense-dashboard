module View.Pathfinder.ConversionDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Pathfinder exposing (sidePanelCss)
import Css.Table
import Html.Styled exposing (Html, div)
import Model.Currency exposing (asset)
import Model.Pathfinder.ConversionDetails exposing (ConversionDetailsModel)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder as Pathfinder
import Msg.Pathfinder.ConversionDetails exposing (ConversionDetailsMsgs(..))
import RecordSetter as Rs
import Svg.Styled.Attributes exposing (css)
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Css exposing (spread)
import Util.View exposing (copyIconPathfinder, copyIconPathfinderAbove, none, timeToCell, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (noTools)
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
        noTools
        (CTable.config tableStyles vc viewState.raw.id isTxOnGraph)
        viewState.table
    ]
        |> div []
        |> subTxsTab


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

        title =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    Locale.string vc.locale "Swap transaction"

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    Locale.string vc.locale "Bridge transaction"
    in
    SidePanelComponents.sidePanelSwapTransactionWithAttributes
        (SidePanelComponents.sidePanelSwapTransactionAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack (closeAttrs Pathfinder.UserClosedDetailsView)
        )
        { identifierWithCopyIcon =
            { identifier = baseTxIdString |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = baseTxIdString |> copyIconPathfinder vc
            , chevronInstance = none
            , addTagIconInstance = none
            }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , root = { subTxListInstance = txTab vc isTxOnGraph viewState, tabsVisible = False }
        , sidePanelSwapHeader = { headerText = title }
        , titleOfInputValue = { infoLabel = Locale.string vc.locale "Input Value" }
        , titleOfOutputValue = { infoLabel = Locale.string vc.locale "Output Value" }
        , titleOfReceiver = { infoLabel = Locale.string vc.locale "receiver" }
        , titleOfSender = { infoLabel = Locale.string vc.locale "sender" }
        , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
        , valueOfInputValue =
            viewState.raw.rawInputTransaction
                |> Tx.getInputValueForAddressFromRawTx cr.fromAddress
                |> valuesToCell vc (asset cr.fromNetwork (viewState.raw.rawInputTransaction |> Tx.getAssetFromRawTx))
        , valueOfOutputValue =
            viewState.raw.rawOutputTransaction
                |> Tx.getOutputValueForAddressFromRawTx cr.toAddress
                |> valuesToCell vc (asset cr.fromNetwork (viewState.raw.rawOutputTransaction |> Tx.getAssetFromRawTx))
        , valueOfReceiver = { copyIconInstance = copyIconPathfinderAbove vc cr.toAddress, firstRowText = cr.toAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfSender = { copyIconInstance = copyIconPathfinderAbove vc cr.fromAddress, firstRowText = cr.fromAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfTimestamp = viewState.raw.rawOutputTransaction |> getTimestamp |> timeToCell vc
        }
