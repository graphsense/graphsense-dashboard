module Init.Pathfinder.Table.TransactionTable exposing (init)

import Api.Data
import Api.Request.Addresses
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import Init.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import Util.Data exposing (timestampToPosix)
import Util.ThemedSelectBox as ThemedSelectBox


getCompleteAssetList : List String -> List (Maybe String)
getCompleteAssetList l =
    Nothing :: (l |> List.map Just)


init : Update.Config -> Network -> Id -> Api.Data.Address -> List String -> TransactionTable.Model
init uc network addressId data assets =
    let
        table isDesc =
            Table.initSorted isDesc TransactionTable.titleTimestamp
                |> InfiniteTable.init
                    { pagesize = 25
                    , rowHeight = 38
                    , containerHeight = 300
                    }

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
                            |> DateRangePicker.init UpdateDateRangePicker mmax (Just mn) (Just mmax)
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
