port module Ports exposing (..)

import Json.Encode exposing (Value)


port console : String -> Cmd msg


port exportGraphics : String -> Cmd msg


port deserialize : () -> Cmd msg


port deserialized : (Value -> msg) -> Sub msg
