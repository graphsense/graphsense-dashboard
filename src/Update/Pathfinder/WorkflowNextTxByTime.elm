module Update.Pathfinder.WorkflowNextTxByTime exposing (update)

import Api.Data
import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..), WorkflowNextTxByTimeMsg(..), WorkflowNextTxContext)


update : WorkflowNextTxContext -> WorkflowNextTxByTimeMsg -> Model -> ( Model, List Effect )
update { addressId, direction } msg model =
    case msg of
        BrowserGotBlockHeight blockAtDate ->
            --Debug.todo ""
            n model

        BrowserGotRecentTx data ->
            let
                getHash tx =
                    case tx of
                        Api.Data.AddressTxAddressTxUtxo t ->
                            t.txHash

                        Api.Data.AddressTxTxAccount t ->
                            t.txHash
            in
            ( model
            , data.addressTxs
                |> List.head
                |> Maybe.map getHash
                |> Maybe.map
                    (\txHash ->
                        BrowserGotTxForAddress addressId direction
                            |> Api.GetTxEffect
                                { currency = Id.network addressId
                                , txHash = txHash
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                            |> ApiEffect
                            |> List.singleton
                    )
                |> Maybe.withDefault []
            )
