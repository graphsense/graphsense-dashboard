module View.Settings exposing (..)

import Config.View exposing (Config)
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import View.Locale as Locale


view : Config -> Html msg
view vc =
    h2
        [ Css.View.heading2 vc |> css
        ]
        [ Locale.text vc.locale "Settings"
        ]
