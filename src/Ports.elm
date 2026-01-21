port module Ports exposing (console, deserialize, deserialized, exportGraphImage, exportGraphPdf, exportGraphics, newTab, pluginsIn, pluginsOut, saveToLocalStorage, serialize, setDirty, toClipboard, uncaughtError)

import Json.Encode exposing (Value)


port console : String -> Cmd msg


port exportGraphics : String -> Cmd msg


port exportGraphImage : String -> Cmd msg


port exportGraphPdf : String -> Cmd msg


port deserialize : () -> Cmd msg


port deserialized : (( String, Value ) -> msg) -> Sub msg


port serialize : ( String, Value ) -> Cmd msg


port pluginsOut : Value -> Cmd msg


port pluginsIn : (( String, Value ) -> msg) -> Sub msg


port newTab : String -> Cmd msg


port toClipboard : String -> Cmd msg


port setDirty : Bool -> Cmd msg


port saveToLocalStorage : Value -> Cmd msg


port uncaughtError : (Value -> msg) -> Sub msg



--port loadFromLocalStorage : String -> Cmd msg
