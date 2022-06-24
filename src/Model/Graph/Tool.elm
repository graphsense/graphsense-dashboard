module Model.Graph.Tool exposing (..)

import Color exposing (Color)
import Config.Graph exposing (Config)
import Html exposing (Html)
import Model.Graph.Highlighter as Highlighter
import Model.Graph.Legend as Legend


type alias Tool msg =
    { icon : Html msg
    , title : String
    , msg : String -> msg
    , color : Maybe Color
    , status : Status
    }


type Toolbox
    = Legend (List Legend.Item)
    | Configuration Config
    | Export
    | Import
    | Highlighter


type Status
    = Active
    | Inactive
    | Disabled


isLegend : Toolbox -> Bool
isLegend tb =
    case tb of
        Legend _ ->
            True

        _ ->
            False


isConfiguration : Toolbox -> Bool
isConfiguration tb =
    case tb of
        Configuration _ ->
            True

        _ ->
            False


isExport : Toolbox -> Bool
isExport tb =
    case tb of
        Export ->
            True

        _ ->
            False


isImport : Toolbox -> Bool
isImport tb =
    case tb of
        Import ->
            True

        _ ->
            False


isHighlighter : Toolbox -> Bool
isHighlighter tb =
    case tb of
        Highlighter ->
            True

        _ ->
            False
