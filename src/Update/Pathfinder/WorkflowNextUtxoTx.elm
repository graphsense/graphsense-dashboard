module Update.Pathfinder.WorkflowNextUtxoTx exposing (Config, Error(..), Msg, Workflow, start, update)

import Api.Data
import Effect.Api as Api
import Init.Pathfinder.Id as Id
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id as Id exposing (Id)
import Set
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
                anchorStillInInputs =
                    tx.inputs
                        |> Maybe.withDefault []
                        |> List.any (.address >> List.member (Id.id config.addressId))

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
                            if not anchorStillInInputs then
                                False

                            else if hasExternalNonChange then
                                False

                            else if allExternalAreChange then
                                True

                            else
                                -- No positive change signal: stop at current tx.
                                -- This avoids skipping a valid immediate follow-up.
                                not hasExternalOutputs && isOnlyCurrentAddress
            in
            if config.direction == Outgoing && not anchorStillInInputs then
                Workflow.Err NoTxFound

            else if shouldContinue then
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
    tx.outputs
        |> Maybe.withDefault []
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.second >> (.address >> List.member (Id.id addressId)))
        |> List.Extra.maximumBy (Tuple.second >> (.value >> .value))
        |> Maybe.map
            (\( outputPosition, txOutput ) ->
                txOutput.index
                    |> Maybe.withDefault outputPosition
            )
