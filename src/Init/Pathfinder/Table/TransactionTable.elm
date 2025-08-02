module Init.Pathfinder.Table.TransactionTable exposing (init, initWithFilter, loadTxs)

import Api.Data
import Api.Request.Addresses
import Api.Time exposing (Posix)
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Graph.Table
import Model.DateFilter exposing (DateFilterRaw)
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import PagedTable
import Util.Data exposing (timestampToPosix)
import Util.ThemedSelectBox as ThemedSelectBox


itemsPerPage : Int
itemsPerPage =
    5


getCompleteAssetList : List String -> List (Maybe String)
getCompleteAssetList l =
    Nothing :: (l |> List.map Just)


init : Update.Config -> Network -> Maybe DateFilterRaw -> Id -> Api.Data.Address -> List String -> TransactionTable.Model
init uc network datefilterPreset addressId data assets =
    let
        nrItems =
            data.noIncomingTxs + data.noOutgoingTxs

        table isDesc =
            Init.Graph.Table.initSorted isDesc TransactionTable.titleTimestamp
                |> PagedTable.init
                |> PagedTable.setNrItems nrItems
                |> PagedTable.setItemsPerPage itemsPerPage

        ( mmin, mmax ) =
            Address.getActivityRange data
    in
    datefilterPreset
        |> Maybe.map
            (\dfp ->
                let
                    drp =
                        datePickerSettings uc.locale (dfp.fromDate |> Maybe.withDefault mmin) (dfp.toDate |> Maybe.withDefault mmax)
                            |> DateRangePicker.init UpdateDateRangePicker mmax dfp.fromDate dfp.toDate
                in
                initWithFilter data (Just drp) Nothing Nothing assets
            )
        |> Maybe.withDefault
            (Network.getRecentTxForAddress network Incoming addressId
                |> Maybe.map
                    (\tx ->
                        let
                            mn =
                                Tx.getRawTimestamp tx
                                    |> timestampToPosix
                        in
                        { table = table False
                        , order = Just Api.Request.Addresses.Order_Asc
                        , dateRangePicker =
                            datePickerSettings uc.locale mn mmax
                                |> DateRangePicker.init UpdateDateRangePicker mmax Nothing Nothing
                                |> Just
                        , direction = Nothing
                        , isTxFilterViewOpen = False
                        , assetSelectBox = ThemedSelectBox.init (getCompleteAssetList assets)
                        , selectedAsset = Nothing
                        }
                    )
                |> Maybe.withDefault
                    (initWithFilter data Nothing Nothing Nothing assets)
            )


initWithFilter : Api.Data.Address -> Maybe (DateRangePicker.Model Msg) -> Maybe Direction -> Maybe String -> List String -> TransactionTable.Model
initWithFilter data dateFilter direction selectedAsset assets =
    let
        nrItems =
            data.noIncomingTxs + data.noOutgoingTxs

        table isDesc =
            Init.Graph.Table.initSorted isDesc TransactionTable.titleTimestamp
                |> PagedTable.init
                |> PagedTable.setNrItems nrItems
                |> PagedTable.setItemsPerPage itemsPerPage
    in
    { table = table True
    , order = Nothing
    , dateRangePicker = dateFilter
    , direction = direction
    , isTxFilterViewOpen = False
    , assetSelectBox = ThemedSelectBox.init (getCompleteAssetList assets)
    , selectedAsset = selectedAsset
    }


loadTxs : Id -> Maybe Direction -> Maybe Posix -> Maybe Posix -> Maybe String -> Effect
loadTxs addressId direction fromDate toDate selectedAsset =
    (GotTxsForAddressDetails ( fromDate, toDate ) >> AddressDetailsMsg addressId)
        |> Api.GetAddressTxsByDateEffect
            { currency = Id.network addressId
            , address = Id.id addressId
            , direction = direction
            , pagesize = itemsPerPage
            , nextpage = Nothing
            , order = Nothing
            , tokenCurrency = selectedAsset
            , minDate = fromDate
            , maxDate = toDate
            }
        |> ApiEffect
