module Msg.Pathfinder exposing (..)

import Api.Data
import Model.Direction exposing (Direction)
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tools exposing (PointerTool)
import Msg.Pathfinder.AddressDetails as AddressDetails
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
    | UserClickedRestartYes
    | UserClickedUndo
    | UserClickedRedo
    | UserClickedExportGraph
    | UserClickedImportFile
    | UserClosedDetailsView
    | UserPressedCtrlKey
    | UserReleasedCtrlKey
    | UserPressedNormalKey String
    | UserReleasedNormalKey String
    | AddressDetailsMsg AddressDetails.Msg
    | TxDetailsMsg TxDetailsMsg
    | AnimationFrameDeltaForTransform Float
    | AnimationFrameDeltaForMove Float
    | BrowserGotAddressData Id Api.Data.Address
    | BrowserGotAddressTags Id Api.Data.AddressTags
    | UserClickedAddressExpandHandle Id Direction
    | UserClickedAddress Id
    | PluginMsg Plugin.Msg
    | SearchMsg Search.Msg
    | NoOp
    | BrowserGotTxForAddress Id Direction Api.Data.Tx
    | BrowserGotActor String Api.Data.Actor
    | BrowserGotTx Api.Data.Tx
    | ChangedDisplaySettingsMsg DisplaySettingsMsg
    | Tick Posix
    | UserClickedTx Id
    | UserClickedTxCheckboxInTable Api.Data.AddressTx
    | UserClickedAddressCheckboxInTable Id
    | WorkflowNextUtxoTx WorkflowNextTxContext WorkflowNextUtxoTxMsg
    | WorkflowNextTxByTime WorkflowNextTxContext WorkflowNextTxByTimeMsg
    | UserPushesLeftMouseButtonOnUtxoTx Id Coords
    | UserClickedRemoveAddressFromGraph Id
    | UserReleasedDeleteKey


type DisplaySettingsMsg
    = ChangePointerTool PointerTool
    | UserClickedToggleDisplaySettings
    | UserClickedToggleShowTxTimestamp


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
