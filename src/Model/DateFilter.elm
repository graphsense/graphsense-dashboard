module Model.DateFilter exposing (DateFilterRaw, emptyDateFilterRaw, init)

import Time


emptyDateFilterRaw : DateFilterRaw
emptyDateFilterRaw =
    { fromDate = Nothing, toDate = Nothing }


init : Maybe Time.Posix -> Maybe Time.Posix -> DateFilterRaw
init f t =
    { fromDate = f, toDate = t }


type alias DateFilterRaw =
    { fromDate : Maybe Time.Posix, toDate : Maybe Time.Posix }
