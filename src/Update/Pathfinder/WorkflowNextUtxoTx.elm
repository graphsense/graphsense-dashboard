module Update.Pathfinder.WorkflowNextUtxoTx exposing (loadReferencedTx, update)

import Api.Data exposing (Tx(..))
import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Id as Id
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..), WorkflowNextTxContext, WorkflowNextUtxoTxMsg(..))
import Set
import Task
import Tuple exposing (pair)


update : WorkflowNextTxContext -> WorkflowNextUtxoTxMsg -> Model -> ( Model, List Effect )
update context msg model =
    case msg of
        BrowserGotReferencedTxs refs ->
            ( model
            , refs
                |> List.map
                    (\ref ->
                        BrowserGotTxForReferencedTx
                            >> WorkflowNextUtxoTx context
                            |> Api.GetTxEffect
                                { currency = Id.network context.addressId
                                , txHash = ref.txHash
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                            |> ApiEffect
                    )
            )

        BrowserGotTxForReferencedTx (Api.Data.TxTxUtxo tx) ->
            let
                io =
                    (case context.direction of
                        Incoming ->
                            tx.inputs

                        Outgoing ->
                            tx.outputs
                    )
                        |> Maybe.withDefault []
                        |> List.map .address
                        |> List.concat
                        |> List.map (Id.init tx.currency)
                        |> Set.fromList
            in
            if Set.singleton context.addressId == io then
                loadReferencedTx context tx
                    |> List.singleton
                    |> pair model

            else
                Api.Data.TxTxUtxo tx
                    |> Task.succeed
                    |> Task.perform
                        (BrowserGotTxForAddress context.addressId context.direction)
                    |> CmdEffect
                    |> List.singleton
                    |> pair model

        _ ->
            n model


loadReferencedTx : WorkflowNextTxContext -> Api.Data.TxUtxo -> Effect
loadReferencedTx context tx =
    let
        ( listLinkedTxRefs, getIo ) =
            case context.direction of
                Incoming ->
                    ( Api.ListSpendingTxRefsEffect, .inputs )

                Outgoing ->
                    ( Api.ListSpentInTxRefsEffect, .outputs )

        index =
            getIo tx
                |> Maybe.andThen
                    (List.Extra.findIndex
                        (.address >> List.any ((==) (Id.id context.addressId)))
                    )
    in
    BrowserGotReferencedTxs
        >> WorkflowNextUtxoTx context
        |> listLinkedTxRefs
            { currency = tx.currency
            , txHash = tx.txHash
            , index = index
            }
        |> ApiEffect
