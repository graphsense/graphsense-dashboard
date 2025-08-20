module Msg.Pathfinder exposing (AddingAddressConfig, AddingRelationsConfig, AddingTxConfig, DisplaySettingsMsg(..), IoDirection(..), Msg(..), OverlayWindows(..), TextTooltipConfig, TxDetailsMsg(..))

import Api.Data
import Color exposing (Color)
import Hovercard
import Model.Direction exposing (Direction)
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.ContextMenu exposing (ContextMenuType)
import Model.Pathfinder.Conversion exposing (Conversion)
import Model.Pathfinder.Deserialize exposing (Deserializing)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (FindPosition)
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.RelationDetails as RelationDetails
import Msg.Search as Search
import Plugin.Msg as Plugin
import Route.Pathfinder exposing (Route)
import Table
import Time
import Update.Pathfinder.WorkflowNextTxByTime as WorkflowNextTxByTime
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Util.Tag exposing (TooltipContext)


type alias AddingAddressConfig =
    { id : Id
    , pos : FindPosition
    , autoLinkTxInTraceMode : Bool
    }


type alias AddingTxConfig =
    { pos : FindPosition
    , loadAddresses : Bool
    , autoLinkInTraceMode : Bool
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
    | UserMovesMouseOverUtxoTx Id
    | UserMovesMouseOutUtxoTx Id
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
    | UserClickedConversionEdge ( Id, Id ) Conversion
    | UserMovesMouseOverConversionEdge ( Id, Id ) Conversion
    | UserMovesMouseOutConversionEdge ( Id, Id ) Conversion


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
    | UserClickedToggleValueDetail


type TxDetailsMsg
    = UserClickedToggleIoTable IoDirection
    | TableMsg IoDirection Table.State


type IoDirection
    = Inputs
    | Outputs
