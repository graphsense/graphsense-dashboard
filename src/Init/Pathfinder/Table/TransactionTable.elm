module Init.Pathfinder.Table.TransactionTable exposing (init, initWithoutFilter, loadFromDateBlock, loadToDateBlock)

import Api.Data
import Api.Request.Addresses
import Api.Time exposing (Posix)
import Config.DateRangePicker exposing (datePickerSettings)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Graph.Table
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
                  }
                , loadTxs addressId mn mx
                )
            )
        |> Maybe.withDefault
            (initWithoutFilter addressId data)


initWithoutFilter : Id -> Api.Data.Address -> ( TransactionTable.Model, List Effect )
initWithoutFilter addressId data =
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
      , dateRangePicker = Nothing
      , txMinBlock = Nothing
      , txMaxBlock = Nothing
      }
    , (GotTxsForAddressDetails ( Nothing, Nothing ) >> AddressDetailsMsg addressId)
        |> Api.GetAddressTxsEffect
            { currency = Id.network addressId
            , address = Id.id addressId
            , direction = Nothing
            , pagesize = itemsPerPage
            , nextpage = Nothing
            , order = Nothing
            , minHeight = Nothing
            , maxHeight = Nothing
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
