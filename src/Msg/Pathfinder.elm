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
    | UserClickedRestart
    | UserClickedUndo
    | UserClickedRedo
    | UserClickedHighlighter
    | UserClickedExportGraph
    | UserClickedImportFile
    | UserClosedDetailsView
    | UserClickedToggleAddressDetailsTable
    | UserClickedToggleTransactionDetailsTable
    | BrowserGotNewAddress Id Api.Data.Address
    | UserClickedAddressExpandHandle Id Direction
    | PluginMsg Plugin.Msg
    | SearchMsg Search.Msg
    | NoOp
    | BrowserGotRecentTx Id Direction Api.Data.AddressTxs
    | BrowserGotTxForAddress Id Direction Api.Data.Tx
