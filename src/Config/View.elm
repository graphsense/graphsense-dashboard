module Config.View exposing (Config)

import Dict exposing (Dict)
import Html.Styled exposing (Html)
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
