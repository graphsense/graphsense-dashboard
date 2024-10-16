module View.Pathfinder.Utils exposing (dateFromTimestamp)

import Config.View as View
import Css.Pathfinder exposing (dateStyle)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import View.Locale as Locale

dateFromTimestamp : View.Config -> Int -> Html msg
dateFromTimestamp vc d =
    let
        date =
            Locale.timestampDateUniform vc.locale d
    in
    div []
        [ div [ dateStyle |> css ] [ text date ] ]
