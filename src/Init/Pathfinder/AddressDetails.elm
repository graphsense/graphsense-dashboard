module Init.Pathfinder.AddressDetails exposing (..)

import Api.Data
import DurationDatePicker exposing (TimePickerVisibility(..))
import Effect.Pathfinder exposing (Effect)
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Locale as Locale
import Model.Pathfinder exposing (Details(..), Selection(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Network exposing (Network)
import Msg.Pathfinder.AddressDetails exposing (Msg(..))


init : Network -> Locale.Model -> Address -> Api.Data.Address -> ( AddressDetails.Model, List Effect )
init network locale address data =
    let
        ( txs, eff ) =
            TransactionTable.init network locale address data
    in
    ( { neighborsTableOpen = False
      , transactionsTableOpen = False
      , txs = txs
      , neighborsOutgoing = NeighborsTable.init data.outDegree
      , neighborsIncoming = NeighborsTable.init data.inDegree
      , address = address
      , data = data
      }
    , eff
    )
