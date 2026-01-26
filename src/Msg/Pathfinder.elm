module Msg.Pathfinder exposing (AddingAddressConfig, AddingRelationsConfig, AddingTxConfig, DisplaySettingsMsg(..), IoDirection(..), Msg(..), OverlayWindows(..), TextTooltipConfig, TxDetailsMsg(..))

import Api.Data
import Color exposing (Color)
import Components.InfiniteTable as InfiniteTable
import Hovercard
import Model.Direction exposing (Direction)
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.ContextMenu exposing (ContextMenuType)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Deserialize exposing (Deserializing)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (FindPosition)
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.ConversionDetails as ConversionDetails
import Msg.Pathfinder.RelationDetails as RelationDetails
import Msg.Search as Search
import Plugin.Msg as Plugin
import Route.Pathfinder exposing (Route)
import Table
import Time
import Update.Pathfinder.WorkflowNextTxByTime as WorkflowNextTxByTime
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Util.Tag exposing (TooltipContext)
import Util.ThemedSelectBox as ThemedSelectBox


type alias AddingAddressConfig =
    { id : Id
    , pos : FindPosition
    , autoLinkTxInTraceMode : Bool
    }


type alias AddingTxConfig =
    { pos : FindPosition
    , loadAddresses : Bool
    , autoLinkInTraceMode : Bool
    , requestedTxHash : String
    }


type alias AddingRelationsConfig =
    { id : Id
    , dir : Direction
    , requestIds : List Id
    , autoLinkInTraceMode : Bool
    }


type Msg
    = UserClickedGraph (Dragging Id)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserPushesLeftMouseButtonOnAddress Id Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | UserToggleAnnotationSettings
    | UserOpensAddressAnnotationDialog Id
    | UserOpensTxAnnotationDialog Id
    | UserClickedRestart
    | UserClickedShowLegend
    | UserClickedToggleHelpDropdown
    | UserClickedRestartYes
    | UserClickedUndo
    | UserClickedRedo
    | UserClosedDetailsView
    | UserPressedModKey
    | UserReleasedModKey
    | UserReleasedEscape
    | UserPressedNormalKey String
    | UserReleasedNormalKey String
    | AddressDetailsMsg Id AddressDetails.Msg
    | ConversionDetailsMsg ( Id, Id ) ConversionDetails.ConversionDetailsMsgs
    | TxDetailsMsg TxDetailsMsg
    | RelationDetailsMsg ( Id, Id ) RelationDetails.Msg
    | AnimationFrameDeltaForTransform Float
    | AnimationFrameDeltaForMove Float
    | BrowserGotAddressData AddingAddressConfig Api.Data.Address
    | BrowserGotClusterData Id Api.Data.Entity
    | BrowserGotAddressesTags (List Id) (List ( Id, Maybe Api.Data.AddressTag ))
    | BrowserGotTagSummary Bool Id Api.Data.TagSummary
    | BrowserGotTagSummaries Bool (List ( Id, Api.Data.TagSummary ))
    | UserClickedAddressExpandHandle Id Direction
    | UserClickedAddress Id
    | PluginMsg Plugin.Msg
    | SearchMsg Search.Msg
    | NoOp
    | BrowserGotActor String Api.Data.Actor
    | BrowserGotTx AddingTxConfig Api.Data.Tx
    | BrowserGotConversionLoop Tx Api.Data.ExternalConversion Api.Data.Tx
    | BrowserGotConversions Tx (List Api.Data.ExternalConversion)
    | ChangedDisplaySettingsMsg DisplaySettingsMsg
    | UserClickedTx Id
    | UserClickedAddressCheckboxInTable Id
    | UserClickedAllAddressCheckboxInTable Direction
    | WorkflowNextUtxoTx WorkflowNextUtxoTx.Config (Maybe Id) WorkflowNextUtxoTx.Msg
    | WorkflowNextTxByTime WorkflowNextTxByTime.Config (Maybe Id) WorkflowNextTxByTime.Msg
    | UserPushesLeftMouseButtonOnUtxoTx Id Coords
    | UserClickedRemoveAddressFromGraph Id
    | UserReleasedDeleteKey
    | UserMovesMouseOverTx Id
    | UserMovesMouseOutTx Id
    | UserMovesMouseOverAddress Id
    | UserMovesMouseOutAddress Id
    | UserMovesMouseOverTagLabel TooltipContext
    | UserMovesMouseOutTagLabel TooltipContext
    | UserMovesMouseOverActorLabel TooltipContext
    | UserMovesMouseOutActorLabel TooltipContext
    | UserInputsAnnotation Id String
    | UserSelectsAnnotationColor Id (Maybe Color)
    | ToolbarHovercardMsg Hovercard.Msg
    | UserClickedExportGraphAsImage String
    | UserClickedExportGraphAsPdf String
    | UserClickedExportGraphTxsAsCSV (Maybe Time.Posix)
    | BrowserGotTagSummariesForExportGraphTxsAsCSV Time.Posix Bool (List ( Id, Api.Data.TagSummary ))
    | UserClickedToolbarDeleteIcon
    | UserClickedFitGraph
    | UserClickedSelectionTool
    | UserClickedSaveGraph (Maybe Time.Posix)
    | UserClickedOpenGraph
    | BrowserGotBulkAddresses (List Api.Data.Address)
    | BrowserGotBulkTxs Deserializing (List ( String, Api.Data.Tx ))
    | UserOpensContextMenu Coords ContextMenuType
    | UserClickedContextMenuDeleteIcon ContextMenuType
    | UserClickedContextMenuOpenInNewTab ContextMenuType
    | UserClickedContextMenuIdToClipboard ContextMenuType
    | UserClickedContextMenuAlignVertically
    | UserClickedContextMenuAlignHorizontally
    | UserClosesContextMenu
    | RuntimePostponedUpdateByRoute Route
    | BrowserWaitedAfterReleasingMouseButton
    | UserOpensDialogWindow OverlayWindows
    | UserGotDataForTagsListDialog Id Api.Data.AddressTags
    | ShowTextTooltip TextTooltipConfig
    | CloseTextTooltip TextTooltipConfig
    | UserClickedToggleTracingMode
    | BrowserGotRelationsToVisibleNeighbors AddingRelationsConfig Api.Data.NeighborAddresses
    | InternalPathfinderAddedAddress Id
    | UserClickedAggEdge ( Id, Id )
    | UserMovesMouseOverAggEdge ( Id, Id )
    | UserMovesMouseOutAggEdge ( Id, Id )
    | UserClickedConversionEdge ( Id, Id ) ConversionEdge
    | UserMovesMouseOverConversionEdge ( Id, Id ) ConversionEdge
    | UserMovesMouseOutConversionEdge ( Id, Id ) ConversionEdge
    | EventualMessagesHeartBeat
    | InternalConversionLoopAddressesLoaded Api.Data.ExternalConversion
    | BrowserGotTxFlow AddingTxConfig Api.Data.Tx Api.Data.Txs


type alias TextTooltipConfig =
    { domId : String, text : String }


type OverlayWindows
    = TagsList Id
    | AddTags Id


type DisplaySettingsMsg
    = UserClickedToggleDisplaySettings
    | UserClickedToggleShowTxTimestamp
    | UserClickedToggleDatesInUserLocale
    | UserClickedToggleShowTimeZoneOffset
    | UserClickedToggleHighlightClusterFriends
    | UserClickedToggleSnapToGrid
    | UserClickedToggleValueDisplay
    | UserClickedToggleBothValueDisplay
    | UserClickedToggleValueDetail
    | UserClickedToggleShowHash


type TxDetailsMsg
    = UserClickedToggleIoTable IoDirection
    | TableMsg IoDirection Table.State
    | TableMsgSubTxTable InfiniteTable.Msg
    | BrowserGotBaseTx Api.Data.Tx
    | BrowserGotTxFlows (Maybe String) Api.Data.Txs
    | UserClickedToggleSubTxsTable
    | UserClickedResetAllSubTxsTableFilters
    | UserClickedResetZeroValueSubTxsTableFilters
    | UserClickedToggleSubTxsTableFilter
    | UserClickedCloseSubTxTableFilterDialog
    | UserClickedTxInSubTxsTable Api.Data.TxAccount
    | NoOpSubTxsTable
    | UserClickedToggleIncludeZeroValueSubTxs
    | SubTxsSelectedAssetSelectBoxMsg (ThemedSelectBox.Msg (Maybe String))


type IoDirection
    = Inputs
    | Outputs
