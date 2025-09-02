module View.Pathfinder.ConversionDetails exposing (view)

import Api.Data
import Config.View as View
import Css.Pathfinder exposing (sidePanelCss)
import Html.Styled exposing (Html)
import Model.Currency exposing (asset)
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder as Pathfinder
import RecordSetter as Rs
import Svg.Styled.Attributes exposing (css)
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Css exposing (spread)
import Util.View exposing (copyIconPathfinder, copyIconPathfinderAbove, none, timeToCell, truncateLongIdentifierWithLengths)
import View.Locale as Locale
import View.Pathfinder.Details exposing (closeAttrs, valuesToCell)


view : View.Config -> Pathfinder.Model -> ( Id, Id ) -> ConversionEdge -> Html Pathfinder.Msg
view vc _ _ viewState =
    let
        baseTxIdString =
            case viewState.rawInputTransaction of
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
            viewState.raw

        title =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    Locale.string vc.locale "Swap Transaction"

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    Locale.string vc.locale "Bridge Transaction"
    in
    SidePanelComponents.sidePanelSwapTransactionWithAttributes
        (SidePanelComponents.sidePanelSwapTransactionAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack closeAttrs
        )
        { identifierWithCopyIcon =
            { identifier = baseTxIdString |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = baseTxIdString |> copyIconPathfinder vc
            , chevronInstance = none
            , addTagIconInstance = none
            }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , root = { subTxListInstance = none, tabsVisible = False }
        , sidePanelSwapHeader = { headerText = title }
        , titleOfInputValue = { infoLabel = Locale.string vc.locale "Input Value" }
        , titleOfOutputValue = { infoLabel = Locale.string vc.locale "Output Value" }
        , titleOfReceiver = { infoLabel = Locale.string vc.locale "Receiver" }
        , titleOfSender = { infoLabel = Locale.string vc.locale "Sender" }
        , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
        , valueOfInputValue =
            viewState.rawInputTransaction
                |> Tx.getInputValueForAddressFromRawTx cr.fromAddress
                |> valuesToCell vc (asset cr.fromNetwork (viewState.rawInputTransaction |> Tx.getAssetFromRawTx))
        , valueOfOutputValue =
            viewState.rawOutputTransaction
                |> Tx.getOutputValueForAddressFromRawTx cr.toAddress
                |> valuesToCell vc (asset cr.fromNetwork (viewState.rawOutputTransaction |> Tx.getAssetFromRawTx))
        , valueOfReceiver = { copyIconInstance = copyIconPathfinderAbove vc cr.toAddress, firstRowText = cr.toAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfSender = { copyIconInstance = copyIconPathfinderAbove vc cr.fromAddress, firstRowText = cr.fromAddress |> truncateLongIdentifierWithLengths 8 4 }
        , valueOfTimestamp = viewState.rawOutputTransaction |> getTimestamp |> timeToCell vc
        }
