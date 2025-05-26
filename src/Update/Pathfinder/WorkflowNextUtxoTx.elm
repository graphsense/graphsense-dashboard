module Update.Pathfinder.WorkflowNextUtxoTx exposing (Config, Error(..), Msg, Workflow, start, update)

import Api.Data
import Effect.Api as Api
import Init.Pathfinder.Id as Id
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id as Id exposing (Id)
import Set
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
            in
            if Set.singleton config.addressId == io then
                if hops > maxHops then
                    Workflow.Err (MaxChangeHopsLimit maxHops tx)

                else
                    continueWorkflow hops config tx

            else
                Workflow.Ok tx

        BrowserGotTxForReferencedTx _ (Api.Data.TxTxAccount _) ->
            Workflow.Err NoTxFound


start : Config -> Api.Data.TxUtxo -> Workflow
start =
    continueWorkflow 0


continueWorkflow : Int -> Config -> Api.Data.TxUtxo -> Workflow
continueWorkflow hops config tx =
    let
        ( listLinkedTxRefs, getIo ) =
            case config.direction of
                Incoming ->
                    ( Api.ListSpendingTxRefsEffect, .inputs )

                Outgoing ->
                    ( Api.ListSpentInTxRefsEffect, .outputs )

        index =
            getIo tx
                |> Maybe.andThen
                    (List.Extra.findIndex
                        (.address >> List.member (Id.id config.addressId))
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
