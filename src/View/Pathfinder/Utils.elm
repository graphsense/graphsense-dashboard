module View.Pathfinder.Utils exposing (dateFromTimestamp, multiLineDateTimeFromTimestamp)

import Config.View as View
import Css.Pathfinder exposing (dateStyle)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Theme.Html.GraphComponents
import View.Locale as Locale


multiLineDateTimeFromTimestamp : View.Config -> Int -> Html msg
multiLineDateTimeFromTimestamp vc d =
    let
        date =
            Locale.timestampDateUniform vc.locale d

        time =
            Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset d
    in
    Theme.Html.GraphComponents.tooltipRowValue
        { tooltipRowValue =
            { firstRow = date
            , secondRow = time
            , secondRowVisible = True
            }
        }


dateFromTimestamp : View.Config -> Int -> Html msg
dateFromTimestamp vc d =
    let
        date =
            Locale.timestampDateUniform vc.locale d
    in
    div []
        [ div [ dateStyle |> css ] [ text date ] ]
