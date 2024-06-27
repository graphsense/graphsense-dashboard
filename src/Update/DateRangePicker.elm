module Update.DateRangePicker exposing (..)


closePicker : Model -> Model
closePicker model =
    { model
        | dateRangePicker = DurationDatePicker.closePicker model.dateRangePicker
        , fromDate = Nothing
        , toDate = Nothing
    }
