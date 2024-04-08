module Msg.Pathfinder exposing (..)

import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import Plugin.Msg as Plugin


type Msg
    = UserClickedGraph (Dragging Id)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserMovesMouseOnGraph Coords
    | PluginMsg Plugin.Msg
