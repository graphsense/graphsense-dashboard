module Model.DateFilter exposing (DateFilterRaw, emptyDateFilterRaw, init, isEmpty)

import Maybe.Extra
import Time


emptyDateFilterRaw : DateFilterRaw
emptyDateFilterRaw =
    { fromDate = Nothing, toDate = Nothing }


init : Maybe Time.Posix -> Maybe Time.Posix -> DateFilterRaw
init f t =
    { fromDate = f, toDate = t }


isEmpty : DateFilterRaw -> Bool
isEmpty df =
    ((df.toDate |> Maybe.Extra.isJust) || (df.fromDate |> Maybe.Extra.isJust)) |> not


type alias DateFilterRaw =
    { fromDate : Maybe Time.Posix, toDate : Maybe Time.Posix }
