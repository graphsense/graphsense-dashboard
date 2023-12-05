module Util.Data exposing (..)

import Api.Data
import List exposing (length)


averageFiatValue : Api.Data.Values -> Float
averageFiatValue { fiatValues } =
    (fiatValues
        |> List.map .value
        |> List.sum
    )
        / (toFloat <| List.length fiatValues)

isAccountLike : String -> Bool
isAccountLike curr = let currl = String.toLower curr
    in
    (currl == "eth" || currl == "trx")