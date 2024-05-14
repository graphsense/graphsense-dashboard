module Msg.Pathfinder exposing (..)

import Api.Data
import Model.Direction exposing (Direction)
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import Msg.Search as Search
import Plugin.Msg as Plugin


type Msg
    = UserClickedGraph (Dragging Id)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | UserClickedRestart
    | UserClickedUndo
    | UserClickedRedo
    | UserClickedHighlighter
    | UserClickedExportGraph
    | UserClickedImportFile
    | UserClosedDetailsView
    | UserClickedToggleAddressDetailsTable
    | UserClickedToggleTransactionDetailsTable
    | AnimationFrameDeltaForTransform Float
    | BrowserGotAddressData Id Api.Data.Address
    | UserClickedAddressExpandHandle Id Direction
    | UserClickedAddress Id
    | PluginMsg Plugin.Msg
    | SearchMsg Search.Msg
    | NoOp
    | BrowserGotRecentTx Id Direction Api.Data.AddressTxs
    | BrowserGotTxForAddress Id Direction Api.Data.Tx
    | BrowserGotTxsForAddressDetails Id Api.Data.AddressTxs
    | BrowserGotActor String Api.Data.Actor
