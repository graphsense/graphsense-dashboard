module Css.DateTimePicker exposing (stylesheet)

import Html.Styled as Html exposing (Html)
import String.Format
import Theme.Colors as Colors


stylesheet : Html msg
stylesheet =
    let
        vars =
            [ ( "edtp-box-shadow", "0px" )
            , ( "edtp-border-radius-lg", "5px" )
            , ( "edtp-border-radius-default", "3px" )
            , ( "edtp-font-size-base", "1rem" )
            , ( "edtp-font-size-sm", "0.875rem" )
            , ( "edtp-font-size-xs", "0.75rem" )
            , ( "edtp-font-size-xxs", "0.625rem" )
            , ( "edtp-icon-button-size", "24px" )
            , ( "edtp-duration-calendars-gap", "1rem" )
            , ( "edtp-transition", "all cubic-bezier(0.4, 0, 0.2, 1) 150ms" )
            , ( "edtp-container-color", Colors.black0 )
            , ( "edtp-container-background-color", "transparent" )
            , ( "edtp-header-text-color", Colors.black0 )
            , ( "edtp-header-chevron-color", Colors.black0 )
            , ( "edtp-header-chevron-background-color", "transparent" )
            , ( "edtp-header-chevron-hover-color", Colors.black0 )
            , ( "edtp-header-chevron-hover-background-color", Colors.grey50 )
            , ( "edtp-header-week-color", Colors.grey200 )
            , ( "edtp-day-size", "42px" )
            , ( "edtp-day-color", Colors.black0 )
            , ( "edtp-day-background-color", "transparent" )
            , ( "edtp-day-hover-color", Colors.black0 )
            , ( "edtp-day-hover-background-color", Colors.grey200 )
            , ( "edtp-day-between-color", Colors.black0 )
            , ( "edtp-day-between-background-color", Colors.oldBrandHighlight )
            , ( "edtp-day-disabled-color", Colors.grey200 )
            , ( "edtp-day-disabled-background-color", "transparent" )
            , ( "edtp-day-picked-color", Colors.white )
            , ( "edtp-day-picked-background-color", Colors.newGreen )
            , ( "edtp-day-today-color", Colors.greenText )
            , ( "edtp-day-today-background-color", "transparent" )
            , ( "edtp-week-number-color", Colors.grey100 )
            , ( "edtp-week-number-background-color", Colors.grey50 )
            , ( "edtp-footer-border-width", "1px" )
            , ( "edtp-footer-border-color", Colors.grey200 )
            , ( "edtp-footer-color", Colors.black0 )
            , ( "edtp-footer-background-color", "transparent" )
            , ( "edtp-footer-empty-color", Colors.grey200 )
            , ( "edtp-footer-font-weight", "normal" )
            , ( "edtp-footer-toggle-button-color", Colors.greenText )
            , ( "edtp-footer-toggle-button-background-color", "transparent" )
            , ( "edtp-footer-toggle-button-hover-color", Colors.greenText )
            , ( "edtp-footer-toggle-button-hover-background-color", Colors.grey50 )
            , ( "edtp-footer-select-color", Colors.black0 )
            , ( "edtp-footer-select-background-color", Colors.white )
            , ( "edtp-footer-select-border-color", Colors.grey200 )
            ]
                |> List.map (\( a, b ) -> "--" ++ a ++ ": " ++ b)
                |> String.join ";\n"
    in
    """
    :root {{{ }}}
    .elm-datetimepicker--picker-container { 
        display: flex;
        flex-flow: column-reverse;
    }
    """
        |> String.Format.value vars
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
