module Msg.Pathfinder exposing (AddingAddressConfig, AddingRelationsConfig, AddingTxConfig, DisplaySettingsMsg(..), Msg(..), OutMsg(..), OverlayWindows(..))

import Api.Data
import Color exposing (Color)
import Components.Tooltip as Tooltip
import Components.TransactionFilter as TransactionFilter
import Hovercard
import Model.Dialog as Dialog
import Model.Direction exposing (Direction)
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.ContextMenu exposing (ContextMenuType)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Deserialize exposing (Deserializing)
import Model.Pathfinder.Id exposing (Id, TxsFilterId)
import Model.Pathfinder.Network exposing (FindPosition)
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.ConversionDetails as ConversionDetails
import Msg.Pathfinder.RelationDetails as RelationDetails
import Msg.Pathfinder.SearchBox as OnGraphSearch
import Msg.Pathfinder.TxDetails as TxDetails
import Msg.Search as Search
import Plugin.Msg as Plugin
import Route.Pathfinder exposing (Route)
import Time
import Update.Pathfinder.WorkflowNextTxByTime as WorkflowNextTxByTime
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Util.TooltipType exposing (TooltipType)


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
    | UserPushesRightMouseButtonOnGraph Coords
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
    | UserPressedHotkey String
    | UserPressedArrowKey Direction
    | UserPressedArrowKeyUp
    | UserPressedArrowKeyDown
    | AddressDetailsMsg Id AddressDetails.Msg
    | ConversionDetailsMsg ( Id, Id ) ConversionDetails.ConversionDetailsMsgs
    | TxDetailsMsg TxDetails.Msg
    | RelationDetailsMsg ( Id, Id ) RelationDetails.Msg
    | AnimationFrameDeltaForTransform Float
    | AnimationFrameDeltaForMove Float
    | BrowserGotAddressData AddingAddressConfig Api.Data.Address
    | BrowserGotAddressPubkeyRelations Id Api.Data.RelatedAddresses
    | BrowserGotAddressDataToRefresh Api.Data.Address
    | BrowserGotPrefetchedAddressData Id Api.Data.Address
      -- the Id is the cluster id the entity request was made with (fresh-aware,
      -- via Data.addressCluster) and is the key of the clusters dict; the
      -- response's .cluster field is normalized to it at the request site
    | BrowserGotClusterData Id Api.Data.Cluster
    | BrowserGotAddressesTags (List Id) (List ( Id, Maybe Api.Data.AddressTag ))
    | BrowserGotTagSummary Bool Id Api.Data.TagSummary
    | BrowserGotClusterTagsProbe Id Bool
    | BrowserGotTagSummaries Bool (List ( Id, Api.Data.TagSummary ))
    | UserClickedAddressExpandHandle Id Direction
    | UserClickedAddressExpandHandleInIoTable Id Id Direction Int
    | UserClickedAddress Id
    | UserClickedCrosschainAddress Id
    | PluginMsg Plugin.Msg
    | SearchMsg Search.Msg
    | UserPressedSearchHotkey
    | OnGraphSearchMsg OnGraphSearch.Msg
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
    | WorkflowNextUtxoTxPrefetch WorkflowNextUtxoTx.Config WorkflowNextUtxoTx.Msg
    | WorkflowNextTxByTimePrefetch WorkflowNextTxByTime.Config WorkflowNextTxByTime.Msg
    | UserPushesLeftMouseButtonOnUtxoTx Id Coords
    | UserClickedRemoveAddressFromGraph Id
    | UserReleasedDeleteKey
    | UserMovesMouseOverTx Id
    | UserMovesMouseOutTx Id
    | UserMovesMouseOverAddress Id
    | UserMovesMouseOutAddress Id
    | UserInputsAnnotation (List Id) String
    | UserSelectsAnnotationColor (List Id) (Maybe Color)
    | UserPushesLeftMouseButtonOnAggEdgeLabel ( Id, Id ) { x : Float, y : Float } Coords
    | ToolbarHovercardMsg Hovercard.Msg
    | UserClickedExportGraph (Maybe Time.Posix)
    | BrowserGotTagSummariesForExportGraphTxsAsCSV Dialog.ExportArea Bool Bool (List ( Id, Api.Data.TagSummary ))
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
    | UserClickedContextMenuAlignHorizontally
    | UserClosesContextMenu
    | RuntimePostponedUpdateByRoute Route
    | BrowserWaitedAfterReleasingMouseButton
    | UserOpensDialogWindow OverlayWindows
    | UserGotDataForTagsListDialog Id Api.Data.AddressTags
    | UserGotMoreAddressTagsForDialog Id Api.Data.AddressTags
    | UserGotClusterTagsForDialog Id Api.Data.AddressTags
    | UserGotMoreClusterTagsForDialog Id Api.Data.AddressTags
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
    | InternalExportGraphTxsCompleted
    | InternalChangedTxFilter TxsFilterId TransactionFilter.Settings
    | InternalHoveredQuickFilter (Maybe TransactionFilter.QuickFilter)
    | TransactionFilterMsg TransactionFilter.Msg
    | TooltipMsg (Tooltip.Msg TooltipType)
    | RepositionTooltip
    | InternalExpandSpecificTxAndAddress Id Id Direction Int


{-| Work the Pathfinder needs the application shell to do.

`Update.Pathfinder` cannot open a dialog, persist user settings or reset the app:
those live on the top-level `Model`. It used to be `Update.elm` that reached in
the other direction, intercepting a dozen `PathfinderMsg` variants _before_ they
reached `Update.Pathfinder` -- which made dispatch depend on the order of case
branches across two files, with three different conventions for whether the
intercepted message was also delegated. Two Pathfinder handlers were unreachable
as a result.

Now every Pathfinder message reaches `updateByMsg`, and anything needing the shell
is returned as one of these instead. See `appLevelOutMsgs`, which is the single
place that decides.

-}
type OutMsg
    = ShowLegendDialog
    | ConfirmRestart
    | Restart
    | SaveUserSettings
    | ChangedDisplaySettings DisplaySettingsMsg
    | OpenTagsListDialog Id Api.Data.AddressTags
    | SetClusterTagsInDialog Id Api.Data.AddressTags
    | AppendAddressTagsInDialog Id Api.Data.AddressTags
    | AppendClusterTagsInDialog Id Api.Data.AddressTags
    | OpenAddTagDialog Id
    | OpenExportDialog (Maybe Time.Posix)
    | CloseExportDialog
    | CloseTopmostOverlay


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
    | UserClickedToggleAvoidOverlapingNodes
