port module Ports exposing (..)

import Json.Encode exposing (Value)


port console : String -> Cmd msg


port exportGraphics : String -> Cmd msg


port deserialize : () -> Cmd msg


port deserialized : (( String, Value ) -> msg) -> Sub msg


port serialize : ( String, Value ) -> Cmd msg


port pluginsOut : Value -> Cmd msg


port pluginsIn : (( String, Value ) -> msg) -> Sub msg


port newTab : String -> Cmd msg


port setDirty : Bool -> Cmd msg


port saveToLocalStorage : Value -> Cmd msg



--port loadFromLocalStorage : String -> Cmd msg
