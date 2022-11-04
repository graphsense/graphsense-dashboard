module Config.Update exposing (Config)

import Color exposing (Color)
import Model.Locale as Locale


type alias Config =
    { defaultColor : Color
    , colorScheme : List Color
    , highlightsColorScheme : List Color
    , locale : Locale.Model
    }
