module Model.Popup exposing (..)

import Draggable exposing (State)


type alias Model =
    { x : Float
    , y : Float
    , id : String
    , size : Maybe ( Float, Float )
    }
