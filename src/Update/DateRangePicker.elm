module Update.DateRangePicker exposing (..)

import DurationDatePicker
import Model.DateRangePicker exposing (Model)
import Tuple exposing (first, second)


closePicker : Model msg -> Model msg
closePicker model =
    { model
        | dateRangePicker = DurationDatePicker.closePicker model.dateRangePicker
    }


resetPicker : Model msg -> Model msg
resetPicker model =
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
                |> Debug.log "update picker"

        mergeTimes newTime oldTime =
            newTime
                |> Maybe.map Just
                |> Maybe.withDefault oldTime
    in
    { model
        | dateRangePicker = newPicker
        , fromDate =
            Maybe.map first maybeRuntime
                |> mergeTimes model.fromDate
        , toDate =
            Maybe.map second maybeRuntime
                |> mergeTimes model.toDate
    }
