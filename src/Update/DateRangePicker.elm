module Update.DateRangePicker exposing (closePicker, openPicker, setFocus, setFrom, setTo, update)

import DurationDatePicker
import Maybe.Extra
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
                model.fromDate
                model.toDate
                model.dateRangePicker
    }


setFrom : Maybe Posix -> Model msg -> Model msg
setFrom fd m =
    { m | fromDate = fd }


setTo : Maybe Posix -> Model msg -> Model msg
setTo fd m =
    { m | toDate = fd }


setFocus : Posix -> Model msg -> Model msg
setFocus fd m =
    { m | focusDate = fd }


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
                |> Maybe.Extra.orElse model.fromDate
        , toDate =
            Maybe.map second maybeRuntime
                |> Maybe.Extra.orElse model.toDate
    }
