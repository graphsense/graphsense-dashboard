module Model.Pathfinder exposing (..)

import Model.Graph exposing (Dragging)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)



--import Route.Pathfinder


type alias Model =
    { network : Maybe Network

    --, route : Route.Pathfinder.Route
    , dragging : Dragging Id
    , selection : List Id
    , transform : Transform.Model Id
    , history : History.Model Entry.Model
    }
