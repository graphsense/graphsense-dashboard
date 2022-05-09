module Model.Graph.Transform exposing (..)


type alias Model =
    { transform :
        { x : Float
        , y : Float
        , z : Float
        }
    , dragging : Dragging
    }


type alias Coords =
    { x : Float, y : Float }


type Dragging
    = NoDragging
    | Dragging Coords Coords
