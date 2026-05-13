port module Ports exposing (blur, console, deserialize, deserialized, exportGraph, exportGraphResult, exportGraphics, exportHotkeyPressed, getBBox, newTab, pluginsIn, pluginsOut, renderedImageForExport, saveHotkeyPressed, saveToLocalStorage, searchHotkeyPressed, sendBBox, serialize, setDirty, toClipboard, uncaughtError)

import Json.Encode exposing (Value)
import Model.Graph.Coords as Coords


port console : String -> Cmd msg


port exportGraphics : String -> Cmd msg


port exportGraph :
    { filename : String
    , graphId : String
    , viewbox : Maybe Coords.BBox
    , transparentBackground : Bool
    }
    -> Cmd msg


port exportGraphResult : (Maybe String -> msg) -> Sub msg


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


port getBBox : ( String, String ) -> Cmd msg


port sendBBox : (Maybe Coords.BBox -> msg) -> Sub msg


port renderedImageForExport : (Bool -> msg) -> Sub msg


port blur : String -> Cmd msg



-- Hotkey ports: used for shortcuts that the browser claims by default
-- (Ctrl+F find bar, Ctrl+S save page, Ctrl+P print). The JS listener in
-- main.js calls preventDefault() and forwards via these ports. Subscribed
-- to in Sub/Pathfinder.elm. Shortcuts without a browser default use
-- Browser.Events directly.


port searchHotkeyPressed : (() -> msg) -> Sub msg


port saveHotkeyPressed : (() -> msg) -> Sub msg


port exportHotkeyPressed : (() -> msg) -> Sub msg
