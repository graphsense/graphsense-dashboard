module Model.DateRangePicker exposing (Model)

import DurationDatePicker
import Time exposing (Posix)


type alias Model msg =
    { settings : DurationDatePicker.Settings
    , dateRangePicker : DurationDatePicker.DatePicker msg
    , fromDate : Maybe Posix
    , toDate : Maybe Posix
    , focusDate : Posix
    }
