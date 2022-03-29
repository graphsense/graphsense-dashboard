module Model exposing (..)


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Increment
    | Decrement
