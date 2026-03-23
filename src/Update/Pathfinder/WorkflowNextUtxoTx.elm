module Update.Pathfinder.WorkflowNextUtxoTx exposing (Config, Error(..), Msg, Workflow, start, update)

import Api.Data
import Effect.Api as Api
import Init.Pathfinder.Id as Id
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id as Id exposing (Id)
import Set exposing (Set)
import Tuple
import Workflow


maxHops : Int
maxHops =
    50


type alias Config =
    { addressId : Id
    , direction : Direction
    }


type Msg
    = BrowserGotReferencedTxs Int (List Api.Data.TxRef)
    | BrowserGotTxForReferencedTx Int Api.Data.Tx


type Error
    = NoTxFound
    | MaxChangeHopsLimit Int Api.Data.TxUtxo


type alias Workflow =
    Workflow.Workflow Api.Data.TxUtxo Msg Error


update : Config -> Msg -> Workflow
update config msg =
    case msg of
        BrowserGotReferencedTxs hops refs ->
            if List.isEmpty refs then
                Workflow.Err NoTxFound

            else
                refs
                    |> List.map
                        (\ref ->
                            BrowserGotTxForReferencedTx hops
                                |> Api.GetTxEffect
                                    { currency = Id.network config.addressId
                                    , txHash = ref.txHash
                                    , includeIo = True
                                    , tokenTxId = Nothing
                                    }
                        )
                    |> Workflow.Next

        BrowserGotTxForReferencedTx hops (Api.Data.TxTxUtxo tx) ->
            let
                io =
                    (case config.direction of
                        Incoming ->
                            tx.inputs

                        Outgoing ->
                            tx.outputs
                    )
                        |> Maybe.withDefault []
                        |> List.concatMap .address
                        |> List.map (Id.init tx.currency)
                        |> Set.fromList

                isOnlyCurrentAddress =
                    Set.singleton config.addressId == io

                legacyOutgoingIndex =
                    findHighestNonSenderOutputIndex tx

                shouldContinue =
                    case config.direction of
                        Incoming ->
                            isOnlyCurrentAddress

                        Outgoing ->
                            let
                                ownAddress =
                                    Id.id config.addressId

                                outputs =
                                    tx.outputs
                                        |> Maybe.withDefault []

                                outputsWithoutOwnAddress =
                                    outputs
                                        |> List.filter
                                            (\output ->
                                                output.address
                                                    |> List.all ((==) ownAddress)
                                                    |> not
                                            )

                                consensusEntries =
                                    tx.heuristics
                                        |> Maybe.andThen .changeHeuristics
                                        |> Maybe.map .consensus
                                        |> Maybe.withDefault []

                                hasConsensus =
                                    not (List.isEmpty consensusEntries)

                                hasExternalOutputs =
                                    not (List.isEmpty outputsWithoutOwnAddress)

                                hasExternalNonChange =
                                    hasConsensus
                                        && List.any (isConsensusChangeOutput consensusEntries >> not) outputsWithoutOwnAddress

                                allExternalAreChange =
                                    hasConsensus
                                        && hasExternalOutputs
                                        && List.all (isConsensusChangeOutput consensusEntries) outputsWithoutOwnAddress
                            in
                            if hasExternalNonChange then
                                False

                            else if allExternalAreChange then
                                True

                            else
                                -- Fall back to legacy behavior when heuristics are missing/partial.
                                case legacyOutgoingIndex of
                                    Just _ ->
                                        True

                                    Nothing ->
                                        isOnlyCurrentAddress
            in
            if shouldContinue then
                if hops > maxHops then
                    Workflow.Err (MaxChangeHopsLimit maxHops tx)

                else
                    continueWorkflow hops config tx

            else
                Workflow.Ok tx

        BrowserGotTxForReferencedTx _ (Api.Data.TxTxAccount _) ->
            Workflow.Err NoTxFound


isConsensusChangeOutput : List Api.Data.ConsensusEntry -> Api.Data.TxValue -> Bool
isConsensusChangeOutput consensusEntries output =
    let
        byAddress =
            List.Extra.find (\entry -> List.member entry.output.address output.address) consensusEntries
    in
    case output.index of
        Just outputIndex ->
            case List.Extra.find (\entry -> entry.output.index == outputIndex) consensusEntries of
                Just _ ->
                    True

                Nothing ->
                    byAddress /= Nothing

        Nothing ->
            byAddress /= Nothing


start : Config -> Api.Data.TxUtxo -> Workflow
start =
    continueWorkflow 0


continueWorkflow : Int -> Config -> Api.Data.TxUtxo -> Workflow
continueWorkflow hops config tx =
    let
        ( listLinkedTxRefs, index ) =
            case config.direction of
                Incoming ->
                    ( Api.ListSpendingTxRefsEffect
                    , findOwnAddressIoIndex config.addressId tx.inputs
                    )

                Outgoing ->
                    ( Api.ListSpentInTxRefsEffect
                    , findOutgoingContinuationIndex config.addressId tx
                    )
    in
    BrowserGotReferencedTxs (hops + 1)
        |> listLinkedTxRefs
            { currency = tx.currency
            , txHash = tx.txHash
            , index = index
            }
        |> List.singleton
        |> Workflow.Next


findOwnAddressIoIndex : Id -> Maybe (List Api.Data.TxValue) -> Maybe Int
findOwnAddressIoIndex addressId values =
    values
        |> Maybe.andThen
            (List.Extra.findIndex
                (.address >> List.member (Id.id addressId))
            )


findOutgoingContinuationIndex : Id -> Api.Data.TxUtxo -> Maybe Int
findOutgoingContinuationIndex addressId tx =
    let
        ownAddressIndex =
            findOwnAddressIoIndex addressId tx.outputs

        legacyOutgoingIndex =
            findHighestNonSenderOutputIndex tx

        consensusEntries =
            tx.heuristics
                |> Maybe.andThen .changeHeuristics
                |> Maybe.map .consensus
                |> Maybe.withDefault []

        senderAddresses =
            tx.inputs
                |> Maybe.withDefault []
                |> List.concatMap .address
                |> Set.fromList

        outputsWithoutSenderAddress =
            tx.outputs
                |> Maybe.withDefault []
                |> List.filter (isNotSenderOutput senderAddresses)

        externalConsensusEntries =
            outputsWithoutSenderAddress
                |> List.filterMap (findConsensusEntryForOutput consensusEntries)

        selectedChangeIndex =
            externalConsensusEntries
                |> List.Extra.maximumBy .confidence
                |> Maybe.map (.output >> .index)
    in
    case selectedChangeIndex of
        Just index ->
            Just index

        Nothing ->
            case legacyOutgoingIndex of
                Just index ->
                    Just index

                Nothing ->
                    ownAddressIndex


findConsensusEntryForOutput : List Api.Data.ConsensusEntry -> Api.Data.TxValue -> Maybe Api.Data.ConsensusEntry
findConsensusEntryForOutput consensusEntries output =
    let
        byAddress =
            List.Extra.find (\txOutput -> List.member txOutput.output.address output.address) consensusEntries
    in
    case output.index of
        Just outputIndex ->
            case List.Extra.find (\txOutput -> txOutput.output.index == outputIndex) consensusEntries of
                Just txOutput ->
                    Just txOutput

                Nothing ->
                    byAddress

        Nothing ->
            byAddress


findHighestNonSenderOutputIndex : Api.Data.TxUtxo -> Maybe Int
findHighestNonSenderOutputIndex tx =
    let
        senderAddresses =
            tx.inputs
                |> Maybe.withDefault []
                |> List.concatMap .address
                |> Set.fromList

        nonSenderOutputs =
            tx.outputs
                |> Maybe.withDefault []
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> isNotSenderOutput senderAddresses)
    in
    nonSenderOutputs
        |> List.Extra.maximumBy (Tuple.second >> (.value >> .value))
        |> Maybe.map
            (\( outputPosition, txOutput ) ->
                txOutput.index
                    |> Maybe.withDefault outputPosition
            )


isNotSenderOutput : Set String -> Api.Data.TxValue -> Bool
isNotSenderOutput senderAddresses txOutput =
    txOutput.address
        |> List.all (\address -> Set.member address senderAddresses)
        |> not
