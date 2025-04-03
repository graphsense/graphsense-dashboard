module Config.Update exposing (Config)

import Api.Data
import Color exposing (Color)
import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale


type alias Config =
    { defaultColor : Color
    , categoryToColor : String -> Color
    , highlightsColorScheme : List Color
    , locale : Locale.Model
    , size : Maybe BBox -- position and size of the main pane
    , allConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    , snapToGrid : Bool
    }
