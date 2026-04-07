module View.Pathfinder.TxDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Char
import Components.Table exposing (Table)
import Components.TransactionFilter as TransactionFilter
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth, sidePanelCss)
import Css.Table
import Css.View
import Dict
import Html.Styled exposing (Html, div)
import Html.Styled.Events exposing (preventDefaultOn, stopPropagationOn)
import Json.Decode
import List.Extra
import Maybe.Extra
import Model.Currency exposing (asset, assetFromBase)
import Model.Graph.Coords as Coords
import Model.Pathfinder as Pathfinder exposing (getHavingTags)
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Tx as Tx exposing (ioToId)
import Model.Pathfinder.TxDetails as TxDetails
import Msg.Pathfinder exposing (IoDirection(..), Msg(..), TxDetailsMsg(..))
import RecordSetter as Rs
import RemoteData
import Svg.Styled.Attributes exposing (css)
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Theme.Html.TagsComponents as TagsComponents
import Util.Css exposing (spread)
import Util.Graph exposing (decodeCoords)
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.View exposing (copyIconPathfinder, copyIconPathfinderAbove, none, timeToCell, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (noTools)
import View.Locale as Locale
import View.Pathfinder.Details exposing (closeAttrs, dataTab, emptyCell, valuesToCell)
import View.Pathfinder.InfiniteTable as InfiniteTable
import View.Pathfinder.Table.IoTable as IoTable exposing (IoColumnConfig)
import View.Pathfinder.Table.SubTxsTable as SubTxsTable


filterConfig : TransactionFilter.FilterHeaderConfig TxDetailsMsg
filterConfig =
    { tag = TransactionFilterMsg
    , exportCsv = Nothing
    }


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
    if viewState.hasSubTxsTable then
        let
            subTxsTab c =
                dataTab
                    { title =
                        SidePanelComponents.sidePanelListHeaderTitleWithAttributes
                            (SidePanelComponents.sidePanelListHeaderTitleAttributes
                                |> Rs.s_root [ spread ]
                            )
                            { root =
                                { label = Locale.string vc.locale "Sub transfers"
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
        [ TransactionFilter.view vc
            (Id.network viewState.tx.id)
            filterConfig
            viewState.subTxsTableFilter
            |> Html.Styled.map TxDetailsMsg
        , InfiniteTable.view vc
            [ css fullWidth, css [ Css.height (Css.px 200) ] ]
            (SubTxsTable.config Css.Table.styles vc { selectedSubTx = viewState.tx |> Tx.getTxIdForTx, isCheckedFn = txExistsFn })
            viewState.subTxsTable
            |> Html.Styled.map TxDetailsMsg
        ]
            |> div [ css [ Css.overflowY Css.auto ] ]
            |> subTxsTab

    else
        none


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
    div []
        [ SidePanelComponents.sidePanelEthTransactionWithAttributes
            (SidePanelComponents.sidePanelEthTransactionAttributes
                |> Rs.s_root
                    [ sidePanelCss
                        |> css
                    ]
                |> Rs.s_sidePanelHeaderText [ spread ]
                |> Rs.s_iconsCloseBlack (closeAttrs UserClosedDetailsView)
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
            , titleOfSender = { infoLabel = Locale.string vc.locale "sender" }
            , valueOfSender =
                { firstRowText = baseTx |> Maybe.map (.fromAddress >> truncateLongIdentifierWithLengths 8 4) |> Maybe.withDefault ""
                , copyIconInstance = orLoadingSpinner (.fromAddress >> copyIconPathfinderAbove vc)
                }
            , titleOfReceiver = { infoLabel = Locale.string vc.locale "receiver" }
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
                { headerText = (Id.network id |> String.toUpper) ++ " " ++ Locale.string vc.locale "Transaction"

                -- baseTx
                --     |> Maybe.map
                --         (Tx.fromApiTxAccount
                --             >> Tx.txTypeToLabel
                --             >> Locale.string vc.locale
                --             >> (++) ((String.toUpper <| Id.network id) ++ " ")
                --         )
                --     |> Maybe.withDefault ""
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
        ]


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

        consensusEntries =
            tx.raw.heuristics
                |> Maybe.andThen .changeHeuristics
                |> Maybe.map .consensus
                |> Maybe.withDefault []

        mixingDetails =
            tx.raw.heuristics
                |> Maybe.andThen .coinjoinHeuristics
                |> Maybe.andThen coinjoinMixingDetails

        txValueCell =
            valuesToCell vc (assetFromBase tx.raw.currency) tx.raw.totalOutput

        mixingCell =
            mixingDetails
                |> Maybe.map
                    (\details ->
                        { firstRowText = details.firstRowText
                        , secondRowText = details.secondRowText
                        , secondRowVisible = details.secondRowVisible
                        }
                    )
                |> Maybe.withDefault
                    { firstRowText = ""
                    , secondRowText = ""
                    , secondRowVisible = False
                    }

        mixingConfidenceBadge =
            mixingDetails
                |> Maybe.andThen .confidence
                |> Maybe.map (confidenceBadge vc)

        sidePanelTxInstancesBase =
            SidePanelComponents.sidePanelTransactionInstances

        sidePanelTxInstances =
            { sidePanelTxInstancesBase
                | n115645PmOfMix = mixingConfidenceBadge
            }
    in
    SidePanelComponents.sidePanelTransactionWithInstances
        (SidePanelComponents.sidePanelTransactionAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelTxDetails [ css fullWidth ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack (closeAttrs UserClosedDetailsView)
        )
        sidePanelTxInstances
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
        , valueOfTxValue = txValueCell
        , sidePanelTxDetails = { showMixingRow = mixingDetails |> Maybe.map (always True) |> Maybe.withDefault False }
        , titleOfMix = { infoLabel = Locale.string vc.locale "mixing type" }
        , valueOfMix = mixingCell
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
                                , getChangeInfo = always Nothing
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
                                , getChangeInfo = consensusChangeInfoForOutput consensusEntries
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


type alias MixingDetails =
    { firstRowText : String
    , secondRowText : String
    , secondRowVisible : Bool
    , confidence : Maybe Int
    }


type alias HeuristicMatch =
    { kind : String
    , confidence : Int
    }


type alias DetectedConfidence a =
    { a | detected : Bool, confidence : Int }


type alias WasabiDetectedConfidence a =
    { a | detected : Bool, confidence : Int, version : String }


type alias CoinjoinConsensus a =
    { a | detected : Bool, confidence : Int, sources : List String }


coinjoinMixingDetails :
    { a
        | consensus : Maybe (CoinjoinConsensus b)
        , joinmarket : Maybe (DetectedConfidence c)
        , wasabi : Maybe (WasabiDetectedConfidence d)
        , whirlpoolCoinjoin : Maybe (DetectedConfidence e)
        , whirlpoolTx0 : Maybe (DetectedConfidence f)
    }
    -> Maybe MixingDetails
coinjoinMixingDetails heuristics =
    let
        highestDetectedHeuristic =
            heuristics
                |> detectedHeuristicMatches
                |> List.sortBy (.confidence >> negate)
                |> List.head

        consensusFallback =
            heuristics.consensus
                |> Maybe.andThen
                    (\consensus ->
                        if consensus.detected then
                            Just
                                { kind =
                                    consensus.sources
                                        |> List.head
                                        |> Maybe.map coinjoinSourceLabel
                                        |> Maybe.withDefault "CoinJoin"
                                , confidence = consensus.confidence
                                }

                        else
                            Nothing
                    )

        selected =
            highestDetectedHeuristic
                |> Maybe.Extra.or consensusFallback
    in
    selected
        |> Maybe.map
            (\entry ->
                { firstRowText = entry.kind
                , secondRowText = ""
                , secondRowVisible = True
                , confidence = Just entry.confidence
                }
            )


detectedHeuristicMatches :
    { a
        | joinmarket : Maybe (DetectedConfidence b)
        , wasabi : Maybe (WasabiDetectedConfidence c)
        , whirlpoolCoinjoin : Maybe (DetectedConfidence d)
        , whirlpoolTx0 : Maybe (DetectedConfidence e)
    }
    -> List HeuristicMatch
detectedHeuristicMatches heuristics =
    [ heuristics.joinmarket
        |> Maybe.andThen
            (\h ->
                if h.detected then
                    Just { kind = "JoinMarket", confidence = h.confidence }

                else
                    Nothing
            )
    , heuristics.wasabi
        |> Maybe.andThen
            (\h ->
                if h.detected then
                    Just
                        { kind =
                            if String.isEmpty h.version then
                                "Wasabi"

                            else
                                "Wasabi " ++ h.version
                        , confidence = h.confidence
                        }

                else
                    Nothing
            )
    , heuristics.whirlpoolCoinjoin
        |> Maybe.andThen
            (\h ->
                if h.detected then
                    Just { kind = "Whirlpool CoinJoin", confidence = h.confidence }

                else
                    Nothing
            )
    , heuristics.whirlpoolTx0
        |> Maybe.andThen
            (\h ->
                if h.detected then
                    Just { kind = "Whirlpool Tx0", confidence = h.confidence }

                else
                    Nothing
            )
    ]
        |> List.filterMap identity


confidenceBadge : View.Config -> Int -> Html msg
confidenceBadge vc confidence =
    let
        range =
            confidence
                |> toFloat
                |> (/) 100
                |> getConfidenceRangeFromFloat

        levelText =
            case range of
                High ->
                    Locale.string vc.locale "high confidence"

                Medium ->
                    Locale.string vc.locale "medium confidence"

                Low ->
                    Locale.string vc.locale "low confidence"

        levelVariant =
            case range of
                High ->
                    TagsComponents.ConfidenceLevelConfidenceLevelHigh

                Medium ->
                    TagsComponents.ConfidenceLevelConfidenceLevelMedium

                Low ->
                    TagsComponents.ConfidenceLevelConfidenceLevelLow
    in
    TagsComponents.confidenceLevel
        { root =
            { size = TagsComponents.ConfidenceLevelSizeSmall
            , confidenceLevel = levelVariant
            , text = levelText
            }
        }


coinjoinSourceLabel : String -> String
coinjoinSourceLabel source =
    case source of
        "joinmarket" ->
            "JoinMarket"

        "joinmarket_coinjoin" ->
            "JoinMarket"

        "wasabi" ->
            "Wasabi"

        "wasabi_coinjoin" ->
            "Wasabi"

        "whirlpool" ->
            "Whirlpool"

        "whirlpool_coinjoin" ->
            "Whirlpool CoinJoin"

        "all_coinjoin" ->
            "CoinJoin"

        _ ->
            source
                |> String.split "_"
                |> List.map capitalizeWord
                |> String.join " "


capitalizeWord : String -> String
capitalizeWord word =
    case String.uncons word of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            ""


consensusChangeInfoForOutput : List Api.Data.ConsensusEntry -> Api.Data.TxValue -> Maybe { confidence : Float, heuristics : List String }
consensusChangeInfoForOutput consensusEntries output =
    let
        byAddress =
            List.Extra.find (\entry -> List.member entry.output.address output.address) consensusEntries

        matchedEntry =
            case output.index of
                Just outputIndex ->
                    case List.Extra.find (\entry -> entry.output.index == outputIndex) consensusEntries of
                        Just entry ->
                            Just entry

                        Nothing ->
                            byAddress

                Nothing ->
                    byAddress
    in
    matchedEntry
        |> Maybe.map
            (\entry ->
                { confidence = clamp 0 1 (toFloat entry.confidence / 100)
                , heuristics = entry.sources
                }
            )


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
