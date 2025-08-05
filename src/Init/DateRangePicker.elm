module Init.DateRangePicker exposing (init)

import DurationDatePicker exposing (Settings)
import Model.DateRangePicker exposing (..)
import Time exposing (Posix)


init : (DurationDatePicker.Msg -> msg) -> Posix -> Maybe Posix -> Maybe Posix -> Settings -> Model msg
init toMsg focusDate fromDate toDate settings =
    { settings = settings
    , dateRangePicker = DurationDatePicker.init toMsg
    , toDate = toDate
    , fromDate = fromDate
    , focusDate = focusDate
    }
