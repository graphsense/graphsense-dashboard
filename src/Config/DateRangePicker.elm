module Config.DateRangePicker exposing (datePickerSettings, datePickerSettingsWithoutRange)

import DurationDatePicker exposing (TimePickerVisibility(..))
import Model.Locale as Locale
import Time exposing (Posix)
import Time.Extra as Time exposing (Interval(..))
import View.Locale as Locale


datePickerSettings : Locale.Model -> Posix -> Posix -> DurationDatePicker.Settings
datePickerSettings localeModel min max =
    let
        defaults =
            DurationDatePicker.defaultSettings localeModel.zone

        isDateBefore x datetime =
            Time.posixToMillis x > Time.posixToMillis datetime

        toDate z x =
            Time.floor Day z x
    in
    { defaults
        | isDayDisabled =
            \clientZone datetime ->
                isDateBefore (toDate clientZone datetime) (toDate clientZone max)
                    || isDateBefore (toDate clientZone min) (toDate clientZone datetime)
        , focusedDate = Just max
        , dateStringFn = \_ pos -> (pos |> Time.posixToMillis) |> (\x -> x // 1000) |> Locale.date localeModel
        , timePickerVisibility = NeverVisible
        , showCalendarWeekNumbers = True
    }


datePickerSettingsWithoutRange : Locale.Model -> DurationDatePicker.Settings
datePickerSettingsWithoutRange localeModel =
    let
        defaults =
            DurationDatePicker.defaultSettings localeModel.zone
    in
    { defaults
        | isDayDisabled =
            \_ _ ->
                False
        , focusedDate = Nothing
        , dateStringFn = \_ pos -> (pos |> Time.posixToMillis) |> (\x -> x // 1000) |> Locale.date localeModel
        , timePickerVisibility = NeverVisible
        , showCalendarWeekNumbers = True
    }
