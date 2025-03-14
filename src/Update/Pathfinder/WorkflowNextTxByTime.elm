module Update.Pathfinder.WorkflowNextTxByTime exposing (update)

import Api.Data
import Api.Request.Addresses exposing (Order_(..))
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..), WorkflowNextTxByTimeMsg(..), WorkflowNextTxContext)
import RecordSetter as Rs
import Set
import Update.Pathfinder.Network as Network


update : WorkflowNextTxContext -> WorkflowNextTxByTimeMsg -> Model -> ( Model, List Effect )
update ctx msg model =
    case msg of
        BrowserGotBlockHeight blockAtDate ->
            ( model
            , BrowserGotRecentTx
                >> WorkflowNextTxByTime ctx
                |> Api.GetAddressTxsEffect
                    { currency = Id.network ctx.addressId
                    , address = Id.id ctx.addressId
                    , direction = Just ctx.direction
                    , pagesize = 1
                    , nextpage = Nothing
                    , order =
                        Just
                            (case ctx.direction of
                                Outgoing ->
                                    Order_Asc

                                Incoming ->
                                    Order_Desc
                            )
                    , minHeight =
                        case ctx.direction of
                            Outgoing ->
                                blockAtDate.beforeBlock

                            Incoming ->
                                Nothing
                    , maxHeight =
                        case ctx.direction of
                            Outgoing ->
                                Nothing

                            Incoming ->
                                blockAtDate.beforeBlock
                    }
                |> ApiEffect
                |> List.singleton
            )

        BrowserGotRecentTx data ->
            let
                getTxId tx =
                    case tx of
                        Api.Data.AddressTxAddressTxUtxo t ->
                            t.txHash

                        Api.Data.AddressTxTxAccount t ->
                            t.identifier

                noResults =
                    List.isEmpty data.addressTxs

                net =
                    if noResults then
                        Network.updateAddress ctx.addressId (Address.txsSetter ctx.direction (Address.Txs Set.empty)) model.network

                    else
                        model.network
            in
            ( model |> Rs.s_network net
            , data.addressTxs
                |> List.head
                |> Maybe.map getTxId
                |> Maybe.map
                    (\txId ->
                        BrowserGotTxForAddress ctx.addressId ctx.direction
                            |> Api.GetTxEffect
                                { currency = Id.network ctx.addressId
                                , txHash = txId
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                            |> ApiEffect
                            |> List.singleton
                    )
                |> Maybe.withDefault []
            )
