module Msg.Pathfinder.AddressDetails exposing (Msg(..), RelatedAddressTypes(..), RelatedAddressesTooltipMsgs(..), TooltipContext, TooltipMsgs(..), relatedAddressTypeOptions)

import Api.Data
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.PagedTable as PagedTable
import DurationDatePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Table
import Util.Tag as Tag
import Util.ThemedSelectBox as ThemedSelectBox
import View.Pathfinder.TransactionFilter as TransactionFilter


type RelatedAddressTypes
    = Pubkey
    | MultiInputCluster


relatedAddressTypeOptions : List RelatedAddressTypes
relatedAddressTypeOptions =
    [ MultiInputCluster
    , Pubkey
    ]


type alias TooltipContext =
    { text : String, domId : String }


type RelatedAddressesTooltipMsgs
    = ShowRelatedAddressesTooltip TooltipContext
    | HideRelatedAddressesTooltip TooltipContext
    | ShowTextTooltip TooltipContext
    | HideTextTooltip TooltipContext


type TooltipMsgs
    = RelatedAddressesTooltipMsg RelatedAddressesTooltipMsgs
    | TagTooltipMsg Tag.Msg


type Msg
    = UserClickedToggleTokenBalancesSelect
    | UserClickedToggleTransactionTable
    | UserClickedToggleNeighborsTable Direction
    | UserClickedToggleBalanceDetails
    | UserClickedToggleTotalReceivedDetails
    | UserClickedToggleTotalSpentDetails
    | UserClickedToggleClusterDetailsOpen
    | UserClickedToggleDisplayAllTagsInDetails
    | TransactionsTableSubTableMsg InfiniteTable.Msg
    | NeighborsTableSubTableMsg Direction InfiniteTable.Msg
    | GotTxsForAddressDetails (Maybe String) Api.Data.AddressTxs
    | GotNeighborsForAddressDetails Direction (Maybe String) Api.Data.NeighborAddresses
    | ToggleTxFilterView
    | CloseTxFilterView
    | TransactionFilterMsg TransactionFilter.Msg
    | TableMsg Table.State
    | RelatedAddressesTableMsg Table.State
    | RelatedAddressesPubkeyTableMsg Table.State
    | BrowserGotEntityAddressesForRelatedAddressesTable Api.Data.EntityAddresses
    | BrowserGotEntityAddressTagsForRelatedAddressesTable String Api.Data.AddressTags
    | UserClickedToggleRelatedAddressesTable
    | RelatedAddressesTableSubTableMsg InfiniteTable.Msg
    | RelatedAddressesPubkeyTablePagedTableMsg PagedTable.Msg
    | UserClickedAddressCheckboxInTable Id
    | UserClickedAggEdgeCheckboxInTable Direction Id Api.Data.NeighborAddress
    | UserClickedTxCheckboxInTable Api.Data.AddressTx
    | UserClickedAllTxCheckboxInTable
    | UserClickedTx Id
    | NoOp
    | BrowserGotAddressesForTags Api.Data.AddressTags (List Api.Data.Address)
    | BrowserGotPubkeyRelations Api.Data.RelatedAddresses
    | TooltipMsg TooltipMsgs
    | RelatedAddressesVisibleTableSelectBoxMsg (ThemedSelectBox.Msg RelatedAddressTypes)
    | ExportCSVMsg TransactionTable.Model ExportCSV.Msg
    | GotAddressTxsForExport TransactionTable.Model Api.Data.AddressTxs
    | BrowserGotBulkTxsForExport TransactionTable.Model (List Api.Data.AddressTxUtxo) (Maybe String) Int (List ( String, Api.Data.Tx )) (List ( String, Api.Data.Tx ))
    | BrowserGotBulkTagsForExport TransactionTable.Model ( List Api.Data.TxAccount, Maybe String ) Bool (List ( Id, Api.Data.TagSummary ))
