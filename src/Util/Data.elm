module Util.Data exposing (..)

import Api.Data
import Time


timestampToPosix : Int -> Time.Posix
timestampToPosix =
    (*) 1000
        >> Time.millisToPosix


averageFiatValue : Api.Data.Values -> Float
averageFiatValue { fiatValues } =
    (fiatValues
        |> List.map .value
        |> List.sum
    )
        / (toFloat <| List.length fiatValues)


isAccountLike : String -> Bool
isAccountLike network =
    let
        currl =
            String.toLower network
    in
    currl == "eth" || currl == "trx"


negateTxValue : Api.Data.TxValue -> Api.Data.TxValue
negateTxValue tv =
    let
        negateRate =
            \v -> { code = v.code, value = -v.value }

        negateValues =
            \v -> { value = -v.value, fiatValues = List.map negateRate v.fiatValues }
    in
    { address = tv.address, value = negateValues tv.value }
