module Update.Pathfinder.WorkflowNextUtxoTx exposing (loadReferencedTx, update)

import Api.Data
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Id as Id
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Address exposing (Txs(..), txsSetter)
import Model.Pathfinder.Error exposing (Error(..), InfoError(..))
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..), WorkflowNextTxContext, WorkflowNextUtxoTxMsg(..))
import RecordSetter exposing (s_network)
import Set
import Task
import Tuple exposing (pair)
import Update.Pathfinder.Network as Network
import Util exposing (n)


update : WorkflowNextTxContext -> WorkflowNextUtxoTxMsg -> Model -> ( Model, List Effect )
update context msg model =
    case msg of
        BrowserGotReferencedTxs refs ->
            if List.isEmpty refs then
                ( model
                    |> s_network (Network.updateAddress context.addressId (Txs Set.empty |> txsSetter context.direction) model.network)
                , NoAdjaccentTxForAddressFound context.addressId
                    |> InfoError
                    |> ErrorEffect
                    |> List.singleton
                )

            else
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
                        |> List.concatMap .address
                        |> List.map (Id.init tx.currency)
                        |> Set.fromList
            in
            if Set.singleton context.addressId == io then
                if context.hops > 50 then
                    model
                        |> s_network
                            (Network.updateAddress context.addressId (TxsLastCheckedChangeTx tx |> txsSetter context.direction) model.network)
                        |> n

                else
                    ( model
                    , loadReferencedTx { context | hops = context.hops + 1 } tx
                        |> List.singleton
                    )

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
                        (.address >> List.member (Id.id context.addressId))
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
