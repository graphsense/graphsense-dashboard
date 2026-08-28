port module Ports exposing (blur, console, deserialize, deserializeFile, deserialized, exportGraph, exportGraphResult, getBBox, newTab, pluginsIn, pluginsOut, renderedImageForExport, saveToLocalStorage, sendBBox, serialize, setDirty, toClipboard, uncaughtError, windowBlurred)

import Json.Encode exposing (Value)
import Model.Graph.Coords as Coords


port console : String -> Cmd msg


port exportGraph :
    { filename : String
    , graphId : String
    , viewbox : Maybe Coords.BBox
    , transparentBackground : Bool
    }
    -> Cmd msg


port exportGraphResult : (Maybe String -> msg) -> Sub msg


port deserialize : () -> Cmd msg


{-| Load a dropped .gs File object through the same pipeline as the file picker.
-}
port deserializeFile : Value -> Cmd msg


port deserialized : (( String, Value ) -> msg) -> Sub msg


port serialize : ( String, Value ) -> Cmd msg


port pluginsOut : Value -> Cmd msg


port pluginsIn : (( String, Value ) -> msg) -> Sub msg


port newTab : String -> Cmd msg


port toClipboard : String -> Cmd msg


port setDirty : Bool -> Cmd msg


port saveToLocalStorage : Value -> Cmd msg


port uncaughtError : (Value -> msg) -> Sub msg


port getBBox : ( String, String ) -> Cmd msg


port sendBBox : (Maybe Coords.BBox -> msg) -> Sub msg


port renderedImageForExport : (Bool -> msg) -> Sub msg


port blur : String -> Cmd msg


port windowBlurred : (() -> msg) -> Sub msg
