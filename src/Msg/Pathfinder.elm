module Msg.Pathfinder exposing (..)

import Api.Data
import DurationDatePicker as DatePicker
import Model.Direction exposing (Direction)
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tools exposing (PointerTool)
import Msg.Search as Search
import Plugin.Msg as Plugin
import Time exposing (Posix)


type Msg
    = UserClickedGraph (Dragging Id)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserPushesLeftMouseButtonOnAddress Id Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | UserClickedRestart
    | UserClickedUndo
    | UserClickedRedo
    | UserClickedHighlighter
    | UserClickedExportGraph
    | UserClickedImportFile
    | UserClosedDetailsView
    | UserPressedCtrlKey
    | UserReleasedCtrlKey
    | AddressDetailsMsg AddressDetailsMsg
    | TxDetailsMsg TxDetailsMsg
    | AnimationFrameDeltaForTransform Float
    | AnimationFrameDeltaForMove Float
    | BrowserGotAddressData Id Api.Data.Address
    | UserClickedAddressExpandHandle Id Direction
    | UserClickedAddress Id
    | PluginMsg Plugin.Msg
    | SearchMsg Search.Msg
    | NoOp
    | BrowserGotTxForAddress Id Direction Api.Data.Tx
    | BrowserGotActor String Api.Data.Actor
    | BrowserGotTx Api.Data.Tx
    | ChangedDisplaySettingsMsg DisplaySettingsMsg
    | UpdateDateRangePicker DatePicker.Msg
    | OpenDateRangePicker
    | CloseDateRangePicker
    | ResetDateRangePicker
    | Tick Posix
    | BrowserGotFromDateBlock Posix Api.Data.BlockAtDate
    | BrowserGotToDateBlock Posix Api.Data.BlockAtDate
    | UserClickedTx Id
    | UserClickedTxCheckboxInTable Api.Data.AddressTx
    | UserClickedAddressCheckboxInTable Id
    | WorkflowNextUtxoTx WorkflowNextTxContext WorkflowNextUtxoTxMsg
    | WorkflowNextTxByTime WorkflowNextTxContext WorkflowNextTxByTimeMsg
    | UserPushesLeftMouseButtonOnUtxoTx Id Coords


type DisplaySettingsMsg
    = ChangePointerTool PointerTool


type AddressDetailsMsg
    = UserClickedToggleNeighborsTable
    | UserClickedToggleTransactionTable
    | UserClickedNextPageTransactionTable
    | UserClickedPreviousPageTransactionTable
    | UserClickedNextPageNeighborsTable Direction
    | UserClickedPreviousPageNeighborsTable Direction
    | GotTxsForAddressDetails Id Api.Data.AddressTxs
    | GotNeighborsForAddressDetails Id Direction Api.Data.NeighborAddresses


type TxDetailsMsg
    = UserClickedToggleIOTable


type alias WorkflowNextTxContext =
    { addressId : Id
    , direction : Direction
    }


type WorkflowNextUtxoTxMsg
    = BrowserGotReferencedTxs (List Api.Data.TxRef)


type WorkflowNextTxByTimeMsg
    = BrowserGotBlockHeight Api.Data.BlockAtDate
    | BrowserGotRecentTx Api.Data.AddressTxs