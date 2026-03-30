module Init.Pathfinder.Table.TransactionTable exposing (init)

import Api.Data
import Api.Request.Addresses
import Basics.Extra exposing (flip)
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
import Tuple exposing (first, pair, second)
import Util.Data exposing (timestampToPosix)


init : Update.Config -> Network -> Maybe TransactionFilter.Model -> Id -> Api.Data.Address -> List String -> TransactionTable.Model
init uc network txsFilter addressId data assets =
    let
        ( mmin, mmax ) =
            Address.getActivityRange data

        qfFromTx direction tx =
            Tx.getRawTimestamp tx
                |> timestampToPosix
                |> TransactionFilter.initQuickFilter direction
                |> (tx
                        |> Tx.getAccountTx
                        |> Maybe.map (.raw >> .currency)
                        |> Maybe.map TransactionFilter.quickfilterWithAsset
                        |> Maybe.withDefault identity
                   )

        prefilter =
            Network.getRecentTxForAddress network Incoming addressId
                |> Maybe.map (qfFromTx Outgoing >> flip pair False)

        quickfilters =
            Network.getTxsForAddress network Incoming addressId
                |> List.map (qfFromTx Outgoing)
                |> flip (++)
                    (Network.getTxsForAddress network Outgoing addressId
                        |> Debug.log "outgoing txs"
                        |> List.map (qfFromTx Incoming)
                    )

        isDesc =
            prefilter
                |> Maybe.map second
                |> Maybe.withDefault True
    in
    { table =
        Table.initSorted isDesc TransactionTable.titleTimestamp
            |> InfiniteTable.init "transactionTable" 25
    , order =
        if isDesc then
            Just Api.Request.Addresses.Order_Desc

        else
            Just Api.Request.Addresses.Order_Asc
    , filter =
        txsFilter
            |> Maybe.map (Debug.log "stored")
            |> Maybe.map
                -- on a stored txFilter apply the new date range, asset select box and quickfilters
                (TransactionFilter.withDateRangePicker uc.locale mmin mmax
                    >> TransactionFilter.withAssetSelectBox assets
                    >> flip (List.foldl TransactionFilter.withQuickFilter) quickfilters
                )
            |> Maybe.withDefault
                (TransactionFilter.init
                    |> TransactionFilter.withDateRangePicker uc.locale mmin mmax
                    |> TransactionFilter.withAssetSelectBox assets
                    |> TransactionFilter.withDirection Nothing
                    |> flip (List.foldl TransactionFilter.withQuickFilter) quickfilters
                    |> (prefilter
                            |> Maybe.map (first >> TransactionFilter.setSelectedQuickFilter)
                            |> Maybe.withDefault identity
                       )
                )
    , isTxFilterViewOpen = False
    }
