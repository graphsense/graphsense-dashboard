module Model.Pathfinder.DatePicker exposing (..)

import DurationDatePicker exposing (Settings, TimePickerVisibility(..), defaultSettings, defaultTimePickerSettings)
import Model.Locale
import Time exposing (Posix, Zone)
import Time.Extra as Time exposing (Interval(..))
import View.Locale as Locale


isDateBeforeToday : Posix -> Posix -> Bool
isDateBeforeToday today datetime =
    Time.posixToMillis today > Time.posixToMillis datetime


userDefinedRangeDatePickerSettings : Model.Locale.Model -> Posix -> Settings
userDefinedRangeDatePickerSettings localeModel today =
    let
        defaults =
            defaultSettings localeModel.zone
    in
    { defaults
        | isDayDisabled = \clientZone datetime -> isDateBeforeToday (Time.floor Day clientZone datetime) today
        , focusedDate = Just today
        , dateStringFn = \_ pos -> (pos |> Time.posixToMillis) |> (\x -> x // 1000) |> Locale.timestampDateUniform localeModel
        , timePickerVisibility = NeverVisible
        , showCalendarWeekNumbers = True
    }
