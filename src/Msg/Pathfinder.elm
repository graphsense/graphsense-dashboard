module Msg.Pathfinder exposing (DisplaySettingsMsg(..), IoDirection(..), Msg(..), OverlayWindows(..), TextTooltipConfig, TxDetailsMsg(..), WorkflowNextTxByTimeMsg(..), WorkflowNextTxContext, WorkflowNextUtxoTxMsg(..))

import Api.Data
import Color exposing (Color)
import Hovercard
import Model.Direction exposing (Direction)
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.ContextMenu exposing (ContextMenuType)
import Model.Pathfinder.Deserialize exposing (Deserializing)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (FindPosition)
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Search as Search
import Plugin.Msg as Plugin
import Route.Pathfinder exposing (Route)
import Table
import Time
import Util.Tag exposing (TooltipContext)


type Msg
    = UserClickedGraph (Dragging Id)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserPushesLeftMouseButtonOnAddress Id Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | UserToggleAnnotationSettings
    | UserOpensAddressAnnotationDialog Id
    | UserClickedRestart
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
    | AnimationFrameDeltaForTransform Float
    | AnimationFrameDeltaForMove Float
    | BrowserGotAddressData Id FindPosition Api.Data.Address
    | BrowserGotClusterData Id Api.Data.Entity
    | BrowserGotAddressesTags (List Id) (List ( Id, Maybe Api.Data.AddressTag ))
    | BrowserGotTagSummary Bool Id Api.Data.TagSummary
    | BrowserGotTagSummaries Bool (List ( Id, Api.Data.TagSummary ))
    | UserClickedAddressExpandHandle Id Direction
    | UserClickedAddress Id
    | PluginMsg Plugin.Msg
    | SearchMsg Search.Msg
    | NoOp
    | BrowserGotTxForAddress Id Direction Api.Data.Tx
    | BrowserGotActor String Api.Data.Actor
    | BrowserGotTx FindPosition Bool Api.Data.Tx
    | ChangedDisplaySettingsMsg DisplaySettingsMsg
    | UserClickedTx Id
    | UserClickedAddressCheckboxInTable Id
    | WorkflowNextUtxoTx WorkflowNextTxContext WorkflowNextUtxoTxMsg
    | WorkflowNextTxByTime WorkflowNextTxContext WorkflowNextTxByTimeMsg
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
    | UserClickedToggleClusterDetailsOpen
    | UserClickedToggleDisplayAllTagsInDetails
    | UserClickedToolbarDeleteIcon
    | UserClickedFitGraph
    | UserClickedSelectionTool
    | UserClickedSaveGraph (Maybe Time.Posix)
    | UserClickedOpenGraph
    | BrowserGotBulkAddresses (List Api.Data.Address)
    | BrowserGotBulkTxs Deserializing (List Api.Data.Tx)
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
    | BrowserGotRelationsToVisibleNeighbors Id Direction Api.Data.NeighborAddresses
    | BrowserGotTxForVisibleNeighbor Id Direction Id Api.Data.Tx


type alias TextTooltipConfig =
    { domId : String, text : String }


type OverlayWindows
    = TagsList Id


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


type alias WorkflowNextTxContext =
    { addressId : Id
    , direction : Direction
    , hops : Int
    , resultMsg : Api.Data.Tx -> Msg
    }


type WorkflowNextUtxoTxMsg
    = BrowserGotReferencedTxs (List Api.Data.TxRef)
    | BrowserGotTxForReferencedTx Api.Data.Tx


type WorkflowNextTxByTimeMsg
    = BrowserGotBlockHeight Api.Data.BlockAtDate
    | BrowserGotRecentTx Api.Data.AddressTxs
