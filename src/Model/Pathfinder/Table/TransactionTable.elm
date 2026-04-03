module Model.Pathfinder.Table.TransactionTable exposing (Model, filter, getQuickFilters, quickFilterFromTx, titleHash, titleTimestamp, titleValue)

import Api.Data
import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Components.TransactionFilter as TransactionFilter
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Util.Data exposing (timestampToPosix)


type alias Model =
    { table : InfiniteTable.Model Api.Data.AddressTx
    , order : Maybe Api.Request.Addresses.Order_
    , filter : TransactionFilter.Model
    , isTxFilterViewOpen : Bool
    }


titleHash : String
titleHash =
    "TxHash"


titleValue : String
titleValue =
    "Value"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


filter : Table.Filter Api.Data.AddressTx
filter =
    { search =
        \term a ->
            case a of
                Api.Data.AddressTxTxAccount tx ->
                    String.contains term tx.txHash

                Api.Data.AddressTxAddressTxUtxo tx ->
                    String.contains term tx.txHash
    , filter = always True
    }


quickFilterFromTx : Direction -> Tx -> TransactionFilter.QuickFilter
quickFilterFromTx direction tx =
    Tx.getRawTimestamp tx
        |> timestampToPosix
        |> TransactionFilter.initQuickFilter tx.type_ direction


getQuickFilters : Network -> Id -> List TransactionFilter.QuickFilter
getQuickFilters network addressId =
    Network.getTxsForAddress network Incoming addressId
        |> List.map (quickFilterFromTx Outgoing)
        |> flip (++)
            (Network.getTxsForAddress network Outgoing addressId
                |> List.map (quickFilterFromTx Incoming)
            )
