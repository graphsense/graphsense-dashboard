module Init.Pathfinder.AddressDetails exposing (..)

import Api.Data
import DurationDatePicker exposing (TimePickerVisibility(..))
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Locale as Locale
import Model.Pathfinder exposing (Details(..), Selection(..))
import Model.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.AddressDetails exposing (Msg(..))


init : Locale.Model -> Api.Data.Address -> AddressDetails.Model
init locale address =
    { neighborsTableOpen = False
    , transactionsTableOpen = False
    , txs = TransactionTable.init locale address
    , txMinBlock = Nothing
    , txMaxBlock = Nothing
    , neighborsOutgoing = NeighborsTable.init address.outDegree
    , neighborsIncoming = NeighborsTable.init address.inDegree
    , address = address
    }
