module View.Pathfinder.TxDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth, sidePanelCss)
import Css.Table
import Html.Styled exposing (Html)
import List.Extra
import Model.Currency exposing (asset, assetFromBase)
import Model.Graph.Table
import Model.Pathfinder as Pathfinder exposing (getHavingTags)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Tx as Tx
import Model.Pathfinder.TxDetails as TxDetails
import Model.Tx as Tx
import Msg.Pathfinder exposing (IoDirection(..), Msg(..), TxDetailsMsg(..))
import RecordSetter as Rs
import Svg.Styled.Attributes exposing (css)
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Css exposing (spread)
import Util.View exposing (copyIconPathfinder, none, timeToCell, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (noTools)
import View.Locale as Locale
import View.Pathfinder.Details exposing (closeAttrs, dataTab, valuesToCell)
import View.Pathfinder.Table.IoTable as IoTable exposing (IoColumnConfig)


view : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> TxDetails.Model -> Html Msg
view vc _ model id viewState =
    case viewState.tx.type_ of
        Tx.Utxo tx ->
            utxo vc model id viewState tx

        Tx.Account tx ->
            account vc id tx


account : View.Config -> Id -> Tx.AccountTx -> Html Msg
account vc id tx =
    SidePanelComponents.sidePanelEthTransactionWithAttributes
        (SidePanelComponents.sidePanelEthTransactionAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack closeAttrs
        )
        { identifierWithCopyIcon =
            { identifier = Id.id id |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = Id.id id |> copyIconPathfinder vc
            , chevronInstance = none
            }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
        , valueOfTimestamp = timeToCell vc tx.raw.timestamp
        , titleOfEstimatedValue = { infoLabel = Locale.string vc.locale "Value" }
        , valueOfEstimatedValue = valuesToCell vc (asset tx.raw.network tx.raw.currency) tx.value
        , titleOfSender = { infoLabel = Locale.string vc.locale "Sender" }
        , valueOfSender =
            { firstRowText = Id.id tx.from |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = Id.id tx.from |> copyIconPathfinder vc
            }
        , titleOfReceiver = { infoLabel = Locale.string vc.locale "Receiver" }
        , valueOfReceiver =
            { firstRowText = Id.id tx.to |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = Id.id tx.to |> copyIconPathfinder vc
            }
        , root =
            { tabsVisible = False
            }
        , sidePanelEthTxDetails =
            { contractCreationVisible = tx.raw.contractCreation |> Maybe.withDefault False
            }
        , sidePanelTxHeader =
            { headerText =
                tx.raw.identifier
                    |> Tx.parseTxIdentifier
                    |> Maybe.map Tx.txTypeToLabel
                    |> Maybe.withDefault "Transaction"
                    |> Locale.string vc.locale
                    |> (++) ((String.toUpper <| Id.network id) ++ " ")
            }
        , titleOfContractCreation = { infoLabel = Locale.string vc.locale "contract creation" }
        , valueOfContractCreation =
            { firstRowText =
                Locale.string vc.locale <|
                    if tx.raw.contractCreation |> Maybe.withDefault False then
                        "yes"

                    else
                        "no"
            , secondRowText = ""
            , secondRowVisible = False
            }
        }


utxo : View.Config -> Pathfinder.Model -> Id -> TxDetails.Model -> Tx.UtxoTx -> Html Msg
utxo vc model id viewState tx =
    SidePanelComponents.sidePanelTransactionWithAttributes
        (SidePanelComponents.sidePanelTransactionAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelTxDetails [ css fullWidth ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack closeAttrs
        )
        { identifierWithCopyIcon =
            { identifier = Id.id id |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = Id.id id |> copyIconPathfinder vc
            , chevronInstance = none
            }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
        , valueOfTimestamp = timeToCell vc tx.raw.timestamp
        , titleOfTxValue = { infoLabel = Locale.string vc.locale "Value" }
        , valueOfTxValue = valuesToCell vc (assetFromBase tx.raw.currency) tx.raw.totalOutput
        , root =
            { tabsVisible = False
            , inputListInstance =
                dataTab
                    { title =
                        SidePanelComponents.sidePanelListHeaderTitleInputsWithAttributes
                            (SidePanelComponents.sidePanelListHeaderTitleInputsAttributes
                                |> Rs.s_root [ spread ]
                            )
                            { root =
                                { title = Locale.string vc.locale "Sending addresses"
                                , totalNumber = Locale.int vc.locale tx.raw.noInputs
                                }
                            }
                    , content =
                        let
                            ioTableConfig =
                                { network = tx.raw.currency
                                , hasTags = getHavingTags model
                                , isChange = always False
                                }
                        in
                        if viewState.inputsTableOpen then
                            ioTableView vc Inputs model.network viewState.inputsTable ioTableConfig
                                |> Just

                        else
                            Nothing
                    , onClick =
                        UserClickedToggleIoTable Inputs
                            |> TxDetailsMsg
                    }
            , outputListInstance =
                dataTab
                    { title =
                        SidePanelComponents.sidePanelListHeaderTitleOutputsWithAttributes
                            (SidePanelComponents.sidePanelListHeaderTitleOutputsAttributes
                                |> Rs.s_root [ spread ]
                            )
                            { root =
                                { title = Locale.string vc.locale "Receiving addresses"
                                , totalNumber = Locale.int vc.locale tx.raw.noOutputs
                                }
                            }
                    , content =
                        let
                            ioTableConfig =
                                { network = tx.raw.currency
                                , hasTags = getHavingTags model
                                , isChange =
                                    .address
                                        >> List.head
                                        >> Maybe.andThen
                                            (\id_ ->
                                                Maybe.withDefault [] tx.raw.inputs
                                                    |> List.Extra.find (.address >> List.head >> Maybe.map ((==) id_) >> Maybe.withDefault False)
                                            )
                                        >> (/=) Nothing
                                }
                        in
                        if viewState.outputsTableOpen then
                            ioTableView vc Outputs model.network viewState.outputsTable ioTableConfig
                                |> Just

                        else
                            Nothing
                    , onClick =
                        UserClickedToggleIoTable Outputs
                            |> TxDetailsMsg
                    }
            }
        , sidePanelTxHeader =
            { headerText =
                (String.toUpper <| Id.network id) ++ " " ++ Locale.string vc.locale "Transaction"
            }
        }


ioTableView : View.Config -> IoDirection -> Network -> Model.Graph.Table.Table Api.Data.TxValue -> IoColumnConfig -> Html Msg
ioTableView vc dir network table ioColumnConfig =
    let
        isCheckedFn =
            flip Network.hasAddress network

        styles =
            Css.Table.styles
                |> Rs.s_root
                    (Css.Table.styles.root
                        >> flip (++)
                            [ Css.display Css.block
                            , Css.width (Css.pct 100)
                            ]
                    )
    in
    View.Graph.Table.table
        styles
        vc
        [ css [ Css.overflowY Css.auto, Css.maxHeight (Css.px ((vc.size |> Maybe.map .height |> Maybe.withDefault 500) * 0.5)) ] ]
        noTools
        (IoTable.config styles vc dir isCheckedFn ioColumnConfig)
        table
