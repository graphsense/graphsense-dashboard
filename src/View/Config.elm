module View.Config exposing (Config)

import Html.Styled exposing (Html)
import Locale.Model as Locale
import Model exposing (Model)
import Msg exposing (Msg)
import Theme exposing (Theme)


{-| Holds prepared stuff that should not be part of the model
-}
type alias Config =
    { theme : Theme
    , locale : Locale.Model
    }
