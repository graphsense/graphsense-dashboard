module Init.DateRangePicker exposing (init)

import DurationDatePicker exposing (Settings)
import Model.DateRangePicker exposing (..)
import Time exposing (Posix)


init : (DurationDatePicker.Msg -> msg) -> Posix -> Settings -> Model msg
init toMsg maxDate settings =
    { settings = settings
    , dateRangePicker = DurationDatePicker.init toMsg
    , toDate = Nothing
    , fromDate = Nothing
    , maxDate = maxDate
    }
