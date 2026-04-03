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
import Model.Pathfinder.Table.TransactionTable as TransactionTable exposing (getQuickFilters, quickFilterFromTx)
import Tuple exposing (first, pair, second)


init : Update.Config -> Network -> Maybe TransactionFilter.Settings -> Id -> Api.Data.Address -> List String -> TransactionTable.Model
init uc network txsFilter addressId data assets =
    let
        ( mmin, mmax ) =
            Address.getActivityRange data

        quickfilters =
            --getQuickFilters network addressId
            []

        prefilter =
            Network.getRecentTxForAddress network Incoming addressId
                |> Maybe.map (quickFilterFromTx Outgoing >> flip pair False)

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
            |> Maybe.withDefault
                (prefilter
                    |> Maybe.map (first >> TransactionFilter.initSettingsFromQuickFilter)
                    |> Maybe.withDefault
                        (TransactionFilter.initSettings
                            |> TransactionFilter.withDirection Nothing
                        )
                )
            |> TransactionFilter.init
            |> TransactionFilter.withDateRangePicker uc.locale mmin mmax
            |> TransactionFilter.withAssetSelectBox assets
            |> flip (List.foldl TransactionFilter.withQuickFilter) quickfilters
    , isTxFilterViewOpen = False
    }
