module Init.Pathfinder.Table.TransactionTable exposing (init)

import Api.Data
import Api.Request.Addresses
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Components.TransactionFilter as TransactionFilter
import Config.Update as Update
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Util.Data exposing (timestampToPosix)


init : Update.Config -> Network -> Maybe TransactionFilter.Model -> Id -> Api.Data.Address -> List String -> TransactionTable.Model
init uc network txsFilter addressId data assets =
    let
        table isDesc =
            Table.initSorted isDesc TransactionTable.titleTimestamp
                |> InfiniteTable.init "transactionTable" 25

        ( mmin, mmax ) =
            Address.getActivityRange data

        { desc, order, min, selectedAsset } =
            Network.getRecentTxForAddress network Incoming addressId
                |> Maybe.map
                    (\tx ->
                        { desc = False
                        , order = Just Api.Request.Addresses.Order_Asc
                        , min =
                            Tx.getRawTimestamp tx
                                |> timestampToPosix
                        , selectedAsset =
                            tx
                                |> Tx.getAccountTx
                                |> Maybe.map (.raw >> .currency)
                        }
                    )
                |> Maybe.withDefault
                    { desc = True
                    , order = Just Api.Request.Addresses.Order_Desc
                    , min = mmin
                    , selectedAsset = Nothing
                    }
    in
    { table = table desc
    , order = order
    , filter =
        txsFilter
            |> Maybe.map
                (TransactionFilter.withDateRangePicker uc.locale min mmax
                    >> TransactionFilter.withAssetSelectBox assets
                )
            |> Maybe.withDefault
                (TransactionFilter.init
                    |> TransactionFilter.withDateRangePicker uc.locale min mmax
                    |> TransactionFilter.withAssetSelectBox assets
                    |> TransactionFilter.withDirection Nothing
                    |> TransactionFilter.updateSelectedAsset selectedAsset
                )
    , isTxFilterViewOpen = False
    }
