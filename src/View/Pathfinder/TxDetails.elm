module View.Pathfinder.TxDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Components.Table exposing (Table)
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth, sidePanelCss)
import Css.Table
import Css.View
import Dict
import Html.Styled exposing (Html, div, text)
import Html.Styled.Events exposing (preventDefaultOn, stopPropagationOn)
import Json.Decode
import List.Extra
import Model.Currency exposing (asset, assetFromBase)
import Model.Graph.Coords as Coords
import Model.Pathfinder as Pathfinder exposing (getHavingTags)
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Tx as Tx exposing (ioToId)
import Model.Pathfinder.TxDetails as TxDetails
import Model.Tx as Tx
import Msg.Pathfinder exposing (IoDirection(..), Msg(..), TxDetailsMsg(..))
import RecordSetter as Rs
import RemoteData
import Svg.Styled.Attributes exposing (css)
import Theme.Html.Icons as HIcons
import Theme.Html.SelectionControls as Sc
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Css exposing (spread)
import Util.Graph exposing (decodeCoords)
import Util.View exposing (copyIconPathfinder, copyIconPathfinderAbove, none, timeToCell, truncateLongIdentifierWithLengths)
import View.Controls
import View.Graph.Table exposing (noTools)
import View.Locale as Locale
import View.Pathfinder.Details exposing (closeAttrs, dataTab, emptyCell, valuesToCell)
import View.Pathfinder.InfiniteTable as InfiniteTable
import View.Pathfinder.Table.IoTable as IoTable exposing (IoColumnConfig)
import View.Pathfinder.Table.SubTxsTable as SubTxsTable


view : View.Config -> Pathfinder.Model -> Id -> TxDetails.Model -> Html Msg
view vc model id viewState =
    case viewState.tx.type_ of
        Tx.Utxo tx ->
            utxo vc model id viewState tx

        Tx.Account _ ->
            let
                txExistsFn =
                    \tid -> Dict.member tid model.network.txs
            in
            account vc viewState id txExistsFn


accountAssetList : View.Config -> TxDetails.Model -> (Id -> Bool) -> Html Msg
accountAssetList vc viewState txExistsFn =
    let
        toToggle name selected msg =
            let
                t =
                    Locale.string vc.locale name
            in
            div [] [ text t, View.Controls.toggle { size = Sc.SwitchSizeSmall, disabled = False, selected = selected, msg = msg } ]

        subTxsTab c =
            dataTab
                { title =
                    SidePanelComponents.sidePanelListHeaderTitleWithAttributes
                        (SidePanelComponents.sidePanelListHeaderTitleAttributes
                            |> Rs.s_root [ spread ]
                        )
                        { root =
                            { label = Locale.string vc.locale "Asset Transfers"
                            }
                        }
                , disabled = False
                , content =
                    if viewState.subTxsTableOpen then
                        Just c

                    else
                        Nothing
                , onClick = UserClickedToggleSubTxsTable |> TxDetailsMsg
                }
    in
    [ toToggle "Include Zero Value Txs" viewState.includeZeroValueSubTxs (UserClickedToggleIncludeZeroValueSubTxs |> TxDetailsMsg)
    , InfiniteTable.view vc
        [ css fullWidth, css [ Css.height (Css.px 200) ] ]
        (SubTxsTable.config Css.Table.styles vc { selectedSubTx = viewState.tx |> Tx.getTxIdForTx, isCheckedFn = txExistsFn })
        TableMsgSubTxTable
        viewState.subTxsTable
        |> Html.Styled.map TxDetailsMsg
    ]
        |> div [ css [ Css.overflowY Css.auto ] ]
        |> subTxsTab


account : View.Config -> TxDetails.Model -> Id -> (Id -> Bool) -> Html Msg
account vc viewState id txExistsFn =
    let
        chevronActions =
            div [ stopPropagationOn "click" (Json.Decode.succeed ( Msg.Pathfinder.NoOp, True )) ]
                [ HIcons.iconsChevronDownThinWithAttributes
                    (HIcons.iconsChevronDownThinAttributes
                        |> Rs.s_root
                            [ Util.View.pointer
                            , decodeCoords Coords.Coords
                                |> Json.Decode.map (\c -> ( Msg.Pathfinder.UserOpensContextMenu c (ContextMenu.TransactionIdChevronActions id), True ))
                                |> preventDefaultOn "click"
                            ]
                    )
                    {}
                ]

        baseTx =
            viewState.baseTx |> RemoteData.toMaybe

        orLoadingSpinner f =
            case baseTx of
                Just b ->
                    b |> f

                Nothing ->
                    Util.View.loadingSpinner vc Css.View.loadingSpinner

        baseTxIdString =
            ("0x" ++ Id.id id) |> String.split "_" |> List.head |> Maybe.withDefault ""
    in
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
            { identifier = baseTxIdString |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = baseTxIdString |> copyIconPathfinder vc
            , chevronInstance = chevronActions
            , addTagIconInstance = none
            }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
        , valueOfTimestamp = baseTx |> Maybe.map (timeToCell vc << .timestamp) |> Maybe.withDefault emptyCell
        , titleOfEstimatedValue = { infoLabel = Locale.string vc.locale "Value" }
        , valueOfEstimatedValue = baseTx |> Maybe.map (\b -> valuesToCell vc (asset b.network b.currency) b.value) |> Maybe.withDefault emptyCell
        , titleOfSender = { infoLabel = Locale.string vc.locale "Sender" }
        , valueOfSender =
            { firstRowText = baseTx |> Maybe.map (.fromAddress >> truncateLongIdentifierWithLengths 8 4) |> Maybe.withDefault ""
            , copyIconInstance = orLoadingSpinner (.fromAddress >> copyIconPathfinderAbove vc)
            }
        , titleOfReceiver = { infoLabel = Locale.string vc.locale "Receiver" }
        , valueOfReceiver =
            { firstRowText = baseTx |> Maybe.map (.toAddress >> truncateLongIdentifierWithLengths 8 4) |> Maybe.withDefault ""
            , copyIconInstance = orLoadingSpinner (.toAddress >> copyIconPathfinderAbove vc)
            }
        , root =
            { tabsVisible = False
            , assetListInstance = accountAssetList vc viewState txExistsFn
            , swapsListInstance = none
            }
        , sidePanelEthTxDetails =
            { contractCreationVisible = baseTx |> Maybe.andThen .contractCreation |> Maybe.withDefault False
            }
        , sidePanelTxHeader =
            { headerText =
                baseTx
                    |> Maybe.map
                        (Tx.fromApiTxAccount
                            >> Tx.txTypeToLabel
                            >> Locale.string vc.locale
                            >> (++) ((String.toUpper <| Id.network id) ++ " ")
                        )
                    |> Maybe.withDefault ""
            }
        , titleOfContractCreation = { infoLabel = Locale.string vc.locale "contract creation" }
        , valueOfContractCreation =
            { firstRowText =
                Locale.string vc.locale <|
                    if baseTx |> Maybe.andThen .contractCreation |> Maybe.withDefault False then
                        "yes"

                    else
                        "no"
            , secondRowText = ""
            , secondRowVisible = False
            }
        }


utxo : View.Config -> Pathfinder.Model -> Id -> TxDetails.Model -> Tx.UtxoTx -> Html Msg
utxo vc model id viewState tx =
    let
        chevronActions =
            div [ stopPropagationOn "click" (Json.Decode.succeed ( Msg.Pathfinder.NoOp, True )) ]
                [ HIcons.iconsChevronDownThinWithAttributes
                    (HIcons.iconsChevronDownThinAttributes
                        |> Rs.s_root
                            [ Util.View.pointer
                            , decodeCoords Coords.Coords
                                |> Json.Decode.map (\c -> ( Msg.Pathfinder.UserOpensContextMenu c (ContextMenu.TransactionIdChevronActions id), True ))
                                |> preventDefaultOn "click"
                            ]
                    )
                    {}
                ]
    in
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
            , chevronInstance = chevronActions
            , addTagIconInstance = none
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
                                |> Rs.s_totalNumber
                                    [ css [ Css.property "display" "unset" |> Css.important ] ]
                            )
                            { root =
                                { title = Locale.string vc.locale "Sending addresses"
                                , totalNumber = Locale.int vc.locale tx.raw.noInputs
                                }
                            }
                    , disabled = tx.raw.noInputs == 0
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
                                |> Rs.s_totalNumber
                                    [ css [ Css.property "display" "unset" |> Css.important ] ]
                            )
                            { root =
                                { title = Locale.string vc.locale "Receiving addresses"
                                , totalNumber = Locale.int vc.locale tx.raw.noOutputs
                                }
                            }
                    , disabled = tx.raw.noOutputs == 0
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


ioTableView : View.Config -> IoDirection -> Network -> Table Api.Data.TxValue -> IoColumnConfig -> Html Msg
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

        allChecked =
            table.data
                |> List.map (ioToId ioColumnConfig.network >> Maybe.withDefault ( "", "" ))
                |> List.all isCheckedFn
    in
    View.Graph.Table.table
        styles
        vc
        [ css [ Css.overflowY Css.auto, Css.maxHeight (Css.px ((vc.size |> Maybe.map .height |> Maybe.withDefault 500) * 0.5)) ] ]
        noTools
        (IoTable.config styles vc dir isCheckedFn allChecked ioColumnConfig)
        table
