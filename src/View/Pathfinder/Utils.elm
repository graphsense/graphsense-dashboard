module View.Pathfinder.Utils exposing (dateFromTimestamp, multiLineDateTimeFromTimestamp)

import Config.View as View
import Css.Pathfinder exposing (dateStyle, multiLineDatetimeDateStyle, multiLineDatetimeTimeStyle)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import View.Locale as Locale


multiLineDateTimeFromTimestamp : View.Config -> Int -> Html msg
multiLineDateTimeFromTimestamp vc d =
    let
        date =
            Locale.timestampDateUniform vc.locale d

        time =
            Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset d
    in
    div []
        [ div [ multiLineDatetimeDateStyle |> css ] [ text date ]
        , div [ multiLineDatetimeTimeStyle |> css ] [ text time ]
        ]


dateFromTimestamp : View.Config -> Int -> Html msg
dateFromTimestamp vc d =
    let
        date =
            Locale.timestampDateUniform vc.locale d
    in
    div []
        [ div [ dateStyle |> css ] [ text date ] ]
