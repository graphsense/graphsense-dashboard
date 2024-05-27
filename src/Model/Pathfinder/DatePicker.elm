module Model.Pathfinder.DatePicker exposing (pathfinderRangeDatePickerSettings)

import DurationDatePicker exposing (Settings, TimePickerVisibility(..), defaultSettings, defaultTimePickerSettings)
import Model.Locale
import Time exposing (Posix, Zone)
import Time.Extra as Time exposing (Interval(..))
import View.Locale as Locale


isDateBefore : Posix -> Posix -> Bool
isDateBefore x datetime =
    Time.posixToMillis x > Time.posixToMillis datetime


isDateAfter : Posix -> Posix -> Bool
isDateAfter x datetime =
    Time.posixToMillis x <= Time.posixToMillis datetime


pathfinderRangeDatePickerSettings : Model.Locale.Model -> Posix -> Posix -> Settings
pathfinderRangeDatePickerSettings localeModel min max =
    let
        defaults =
            defaultSettings localeModel.zone
    in
    { defaults
        | isDayDisabled = \clientZone datetime -> isDateBefore (Time.floor Day clientZone datetime) max || isDateAfter (Time.floor Day clientZone datetime) min
        , focusedDate = Just max
        , dateStringFn = \_ pos -> (pos |> Time.posixToMillis) |> (\x -> x // 1000) |> Locale.timestampDateUniform localeModel
        , timePickerVisibility = NeverVisible
        , showCalendarWeekNumbers = True
    }
