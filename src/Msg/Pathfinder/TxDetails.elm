module Msg.Pathfinder.TxDetails exposing (IoDirection(..), Msg(..))

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.TransactionFilter as TransactionFilter
import Table


type IoDirection
    = Inputs
    | Outputs


type Msg
    = UserClickedToggleIoTable IoDirection
    | TableMsg IoDirection Table.State
    | TableMsgSubTxTable InfiniteTable.Msg
    | BrowserGotBaseTx Api.Data.Tx
    | BrowserGotTxFlows (Maybe String) Api.Data.Txs
    | UserClickedToggleSubTxsTable
    | UserClickedTxInSubTxsTable Api.Data.TxAccount
    | TransactionFilterMsg TransactionFilter.Msg
