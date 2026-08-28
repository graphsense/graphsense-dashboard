module Model.Graph exposing (Dragging(..))

import Model.Graph.Coords exposing (Coords)
import Model.Graph.Transform as Transform


type Dragging id
    = NoDragging
    | Dragging (Transform.Model id) Coords Coords
    | DraggingNode id Coords Coords
