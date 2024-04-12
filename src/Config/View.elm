module Config.View exposing (Config, getUnit)

import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale
import Theme.Theme exposing (Theme)


{-| Holds prepared stuff that should not be part of the model
-}
type alias Config =
    { theme : Theme
    , locale : Locale.Model
    , lightmode : Bool
    , size : Maybe BBox -- position and size of the main pane
    }


getUnit : Config -> Float
getUnit { theme } =
    theme.pathfinder.addressRadius * 2
