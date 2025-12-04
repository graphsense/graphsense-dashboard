module Init.Pathfinder.Table.TransactionTable exposing (init)

import Api.Data
import Api.Request.Addresses
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import DurationDatePicker
import Init.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Util.Data exposing (timestampToPosix)
import Util.ThemedSelectBox as ThemedSelectBox


getCompleteAssetList : List String -> List (Maybe String)
getCompleteAssetList l =
    Nothing :: (l |> List.map Just)


init : Update.Config -> Network -> Id -> Api.Data.Address -> List String -> (DurationDatePicker.Msg -> msg) -> TransactionTable.Model msg
init uc network addressId data assets dpMsg =
    let
        table isDesc =
            Table.initSorted isDesc TransactionTable.titleTimestamp
                |> InfiniteTable.init "transactionTable" 25

        ( _, mmax ) =
            Address.getActivityRange data

        ( desc, order, drp ) =
            Network.getRecentTxForAddress network Incoming addressId
                |> Maybe.map
                    (\tx ->
                        let
                            mn =
                                Tx.getRawTimestamp tx
                                    |> timestampToPosix
                        in
                        ( False
                        , Just Api.Request.Addresses.Order_Asc
                        , datePickerSettings uc.locale mn mmax
                            |> DateRangePicker.init dpMsg mmax (Just mn) (Just mmax)
                            |> Just
                        )
                    )
                |> Maybe.withDefault
                    ( True
                    , Just Api.Request.Addresses.Order_Desc
                    , Nothing
                    )
    in
    { table = table desc
    , order = order
    , dateRangePicker = drp
    , direction = Nothing
    , isTxFilterViewOpen = False
    , assetSelectBox = ThemedSelectBox.init (getCompleteAssetList assets)
    , selectedAsset = Nothing
    , includeZeroValueTxs = Nothing -- Backend does not support this filter at the moment
    }
