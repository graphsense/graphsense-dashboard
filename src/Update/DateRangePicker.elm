module Update.DateRangePicker exposing (..)

import DurationDatePicker
import Model.DateRangePicker exposing (Model)
import Tuple exposing (first, second)


closePicker : Model msg -> Model msg
closePicker model =
    { model
        | dateRangePicker = DurationDatePicker.closePicker model.dateRangePicker
        , fromDate = Nothing
        , toDate = Nothing
    }


openPicker : Model msg -> Model msg
openPicker model =
    { model
        | dateRangePicker =
            DurationDatePicker.openPicker model.settings
                model.maxDate
                model.fromDate
                model.toDate
                model.dateRangePicker
    }


update : DurationDatePicker.Msg -> Model msg -> Model msg
update msg model =
    let
        ( newPicker, maybeRuntime ) =
            DurationDatePicker.update model.settings msg model.dateRangePicker
    in
    { model
        | dateRangePicker = newPicker
        , fromDate = Maybe.map first maybeRuntime
        , toDate = Maybe.map second maybeRuntime
    }
