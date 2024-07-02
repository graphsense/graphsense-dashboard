module Init.Pathfinder.Table.TransactionTable exposing (..)

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
import Model.Pathfinder.Address as Address exposing (Address)
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import Util.Data exposing (timestampToPosix)


init : Network -> Locale.Model -> Address -> Api.Data.Address -> ( TransactionTable.Model, List Effect )
init network locale address data =
    let
        itemsPerPage =
            data.noIncomingTxs + data.noOutgoingTxs

        table isDesc =
            { table =
                Init.Graph.Table.initSorted isDesc TransactionTable.titleTimestamp
            , nrItems = Just <| itemsPerPage
            , currentPage = 1
            , itemsPerPage = 5
            }
    in
    Network.getRecentTxForAddress network Incoming (Debug.log "addressId" address.id)
        |> Debug.log "recent tx"
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
                  }
                , loadTxs address.id mn mx
                )
            )
        |> Maybe.withDefault
            ( { table = table True
              , order = Nothing
              , dateRangePicker = Nothing
              }
            , (GotTxsForAddressDetails address.id >> AddressDetailsMsg)
                |> Api.GetAddressTxsEffect
                    { currency = Id.network address.id
                    , address = Id.id address.id
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


loadTxs : Id.Id -> Posix -> Posix -> List Effect
loadTxs id mn mx =
    [ loadToDateBlock id mx
    , loadFromDateBlock id mn
    ]


loadFromDateBlock : Id.Id -> Posix -> Effect
loadFromDateBlock id mx =
    BrowserGotFromDateBlock mx
        >> AddressDetailsMsg
        |> Api.GetBlockByDateEffect
            { currency = Id.network id
            , datetime = mx
            }
        |> ApiEffect


loadToDateBlock : Id.Id -> Posix -> Effect
loadToDateBlock id mn =
    BrowserGotToDateBlock mn
        >> AddressDetailsMsg
        |> Api.GetBlockByDateEffect
            { currency = Id.network id
            , datetime = mn
            }
        |> ApiEffect
