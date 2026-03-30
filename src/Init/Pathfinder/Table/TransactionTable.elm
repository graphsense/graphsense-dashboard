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
import Time
import Tuple exposing (pair)
import Util.Data exposing (timestampToPosix)


init : Update.Config -> Network -> Maybe TransactionFilter.Model -> Id -> Api.Data.Address -> List String -> TransactionTable.Model
init uc network txsFilter addressId data assets =
    let
        table isDesc =
            Table.initSorted isDesc TransactionTable.titleTimestamp
                |> InfiniteTable.init "transactionTable" 25

        ( mmin, mmax ) =
            Address.getActivityRange data

        prefilter =
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

        quickfilters =
            [ Time.millisToPosix 0
                |> TransactionFilter.initQuickFilter Outgoing
            , Time.millisToPosix 0
                |> TransactionFilter.initQuickFilter Incoming
                |> TransactionFilter.quickfilterWithAsset "BLA"
            ]
    in
    { table =
        prefilter
            |> Maybe.map .desc
            |> Maybe.withDefault True
            |> table
    , order =
        prefilter
            |> Maybe.map .order
            |> Maybe.withDefault (Just Api.Request.Addresses.Order_Desc)
    , filter =
        txsFilter
            |> Maybe.map
                (TransactionFilter.withDateRangePicker uc.locale mmin mmax
                    >> TransactionFilter.withAssetSelectBox assets
                )
            |> Maybe.withDefault
                (TransactionFilter.init
                    |> TransactionFilter.withDateRangePicker uc.locale mmin mmax
                    |> TransactionFilter.withAssetSelectBox assets
                    |> TransactionFilter.withDirection Nothing
                    |> flip (List.foldl TransactionFilter.withQuickFilter) quickfilters
                    |> (prefilter
                            |> Maybe.map .selectedAsset
                            |> Maybe.map TransactionFilter.updateSelectedAsset
                            |> Maybe.withDefault identity
                       )
                    |> (prefilter
                            |> Maybe.map .min
                            |> Maybe.map (\mn -> TransactionFilter.updateDateRange ( Just mn, Nothing ))
                            |> Maybe.withDefault identity
                       )
                )
    , isTxFilterViewOpen = False
    }
