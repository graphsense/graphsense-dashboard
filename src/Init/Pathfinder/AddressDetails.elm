module Init.Pathfinder.AddressDetails exposing (..)

import Api.Data
import DurationDatePicker exposing (TimePickerVisibility(..))
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Locale as Locale
import Model.Pathfinder exposing (Details(..), Selection(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.AddressDetails exposing (Msg(..))


init : Locale.Model -> Address -> Api.Data.Address -> AddressDetails.Model
init locale address data =
    { neighborsTableOpen = False
    , transactionsTableOpen = False
    , txs = TransactionTable.init locale address data
    , txMinBlock = Nothing
    , txMaxBlock = Nothing
    , neighborsOutgoing = NeighborsTable.init data.outDegree
    , neighborsIncoming = NeighborsTable.init data.inDegree
    , address = address
    , data = data
    }
