module Model.Pathfinder exposing (..)

import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Search as Search



--import Route.Pathfinder


type alias Model =
    { network : Maybe Network

    --, route : Route.Pathfinder.Route
    , dragging : Dragging Id
    , selection : List Id
    , search : Search.Model
    , transform : Transform.Model Id
    , history : History.Model Entry.Model
    }
