module Update.DateRangePicker exposing (closePicker, openPicker, setFrom, setTo, update)

import DurationDatePicker
import Model.DateRangePicker exposing (Model)
import Time exposing (Posix)
import Tuple exposing (first, second)


closePicker : Model msg -> Model msg
closePicker model =
    { model
        | dateRangePicker = DurationDatePicker.closePicker model.dateRangePicker
    }


openPicker : Model msg -> Model msg
openPicker model =
    { model
        | dateRangePicker =
            DurationDatePicker.openPicker model.settings
                model.focusDate
                (Just model.fromDate)
                (Just model.toDate)
                model.dateRangePicker
    }


setFrom : Posix -> Model msg -> Model msg
setFrom fd m =
    { m | fromDate = fd }


setTo : Posix -> Model msg -> Model msg
setTo fd m =
    { m | toDate = fd }


update : DurationDatePicker.Msg -> Model msg -> Model msg
update msg model =
    let
        ( newPicker, maybeRuntime ) =
            DurationDatePicker.update model.settings msg model.dateRangePicker
    in
    { model
        | dateRangePicker = newPicker
        , fromDate =
            Maybe.map first maybeRuntime
                |> Maybe.withDefault model.fromDate
        , toDate =
            Maybe.map second maybeRuntime
                |> Maybe.withDefault model.toDate
    }
