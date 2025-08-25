module Model.DateFilter exposing (DateFilterRaw, init)

import Time


init : Maybe Time.Posix -> Maybe Time.Posix -> DateFilterRaw
init f t =
    { fromDate = f, toDate = t }


type alias DateFilterRaw =
    { fromDate : Maybe Time.Posix, toDate : Maybe Time.Posix }
