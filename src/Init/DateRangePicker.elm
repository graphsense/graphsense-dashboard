module Init.DateRangePicker exposing (init)

import DurationDatePicker
import Model.DateRangePicker exposing (..)


init : (DurationDatePicker.Msg -> msg) -> Model msg
init toMsg =
    { dateRangePicker = DurationDatePicker.init toMsg
    , toDate = Nothing
    , fromDate = Nothing
    }
