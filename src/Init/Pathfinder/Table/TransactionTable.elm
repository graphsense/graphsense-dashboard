module Init.Pathfinder.Table.TransactionTable exposing (emptyDateFilter, init, initWithFilter, loadFromDateBlock, loadToDateBlock)

import Api.Data
import Api.Request.Addresses
import Api.Time exposing (Posix)
import Config.DateRangePicker exposing (datePickerSettings)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Graph.Table
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import PagedTable
import Util.Data exposing (timestampToPosix)


itemsPerPage : Int
itemsPerPage =
    5


emptyDateFilter : { txMinBlock : Maybe Int, txMaxBlock : Maybe Int, dateRangePicker : Maybe (DateRangePicker.Model Msg) }
emptyDateFilter =
    { txMinBlock = Nothing, txMaxBlock = Nothing, dateRangePicker = Nothing }


init : Network -> Locale.Model -> Id -> Api.Data.Address -> ( TransactionTable.Model, List Effect )
init network locale addressId data =
    let
        nrItems =
            data.noIncomingTxs + data.noOutgoingTxs

        table isDesc =
            Init.Graph.Table.initSorted isDesc TransactionTable.titleTimestamp
                |> PagedTable.init
                |> PagedTable.setNrItems nrItems
                |> PagedTable.setItemsPerPage itemsPerPage
    in
    Network.getRecentTxForAddress network Incoming addressId
        |> Maybe.map
            (\tx ->
                let
                    ( _, mx ) =
                        Address.getActivityRange data

                    mn =
                        Tx.getRawTimestamp tx
                            |> timestampToPosix
                in
                ( { table = table False
                  , order = Just Api.Request.Addresses.Order_Asc
                  , dateRangePicker =
                        datePickerSettings locale mn mx
                            |> DateRangePicker.init UpdateDateRangePicker mn mx
                            |> Just
                  , txMinBlock = Just data.firstTx.height
                  , txMaxBlock = Just data.lastTx.height
                  , direction = Nothing
                  , isTxFilterViewOpen = False
                  }
                , loadTxs addressId mn mx
                )
            )
        |> Maybe.withDefault
            (initWithFilter addressId data emptyDateFilter Nothing)


initWithFilter : Id -> Api.Data.Address -> { x | txMinBlock : Maybe Int, txMaxBlock : Maybe Int, dateRangePicker : Maybe (DateRangePicker.Model Msg) } -> Maybe Direction -> ( TransactionTable.Model, List Effect )
initWithFilter addressId data dateFilter direction =
    let
        nrItems =
            data.noIncomingTxs + data.noOutgoingTxs

        table isDesc =
            Init.Graph.Table.initSorted isDesc TransactionTable.titleTimestamp
                |> PagedTable.init
                |> PagedTable.setNrItems nrItems
                |> PagedTable.setItemsPerPage itemsPerPage
    in
    ( { table = table True
      , order = Nothing
      , dateRangePicker = dateFilter.dateRangePicker
      , txMinBlock = dateFilter.txMinBlock
      , txMaxBlock = dateFilter.txMaxBlock
      , direction = direction
      , isTxFilterViewOpen = False
      }
    , (GotTxsForAddressDetails ( dateFilter.txMinBlock, dateFilter.txMaxBlock ) >> AddressDetailsMsg addressId)
        |> Api.GetAddressTxsEffect
            { currency = Id.network addressId
            , address = Id.id addressId
            , direction = direction
            , pagesize = itemsPerPage
            , nextpage = Nothing
            , order = Nothing
            , tokenCurrency = Nothing
            , minHeight = dateFilter.txMinBlock
            , maxHeight = dateFilter.txMaxBlock
            }
        |> ApiEffect
        |> List.singleton
    )


loadTxs : Id -> Posix -> Posix -> List Effect
loadTxs id mn mx =
    [ loadToDateBlock id mx
    , loadFromDateBlock id mn
    ]


loadFromDateBlock : Id -> Posix -> Effect
loadFromDateBlock id mx =
    BrowserGotFromDateBlock mx
        >> AddressDetailsMsg id
        |> Api.GetBlockByDateEffect
            { currency = Id.network id
            , datetime = mx
            }
        |> ApiEffect


loadToDateBlock : Id -> Posix -> Effect
loadToDateBlock id mn =
    BrowserGotToDateBlock mn
        >> AddressDetailsMsg id
        |> Api.GetBlockByDateEffect
            { currency = Id.network id
            , datetime = mn
            }
        |> ApiEffect
