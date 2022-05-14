module Config.View exposing (Config)

import Html.Styled exposing (Html)
import Model.Locale as Locale
import Theme.Theme exposing (Theme)


{-| Holds prepared stuff that should not be part of the model
-}
type alias Config =
    { theme : Theme
    , locale : Locale.Model
    }
