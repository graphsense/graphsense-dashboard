module Msg.Pathfinder.TxDetails exposing (IoDirection(..), Msg(..))

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Tooltip as Tooltip
import Components.TransactionFilter as TransactionFilter
import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)
import Table
import Util.TooltipType exposing (TooltipType)


type IoDirection
    = Inputs
    | Outputs


type Msg
    = UserClickedToggleIoTable IoDirection
    | TableMsg IoDirection Table.State
    | TableMsgSubTxTable InfiniteTable.Msg
    | IoTableMsg IoDirection InfiniteTable.Msg
    | UserClickedIoTableAddress Id
    | UserClickedIoTableCheckbox Id
    | UserClickedAllIoTableCheckboxes Direction
    | BrowserGotBaseTx Api.Data.Tx
    | BrowserGotTxFlows (Maybe String) Api.Data.Txs
    | UserClickedToggleSubTxsTable
    | UserClickedTxInSubTxsTable Api.Data.TxAccount
    | TransactionFilterMsg TransactionFilter.Msg
    | TooltipMsg (Tooltip.Msg TooltipType)
    | NoOp
