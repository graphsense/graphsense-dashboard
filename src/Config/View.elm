module Config.View exposing (Config)

import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale
import Theme.Theme exposing (Theme)


type alias Config =
    { theme : Theme
    , locale : Locale.Model
    , lightmode : Bool
    , size : Maybe BBox -- position and size of the main pane
    , showDatesInUserLocale : Bool
    }
