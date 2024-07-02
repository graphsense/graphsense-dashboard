module Model.DateRangePicker exposing (..)

import DurationDatePicker
import Time exposing (Posix)


type alias Model msg =
    { settings : DurationDatePicker.Settings
    , dateRangePicker : DurationDatePicker.DatePicker msg
    , fromDate : Posix
    , toDate : Posix
    , focusDate : Posix
    }
