module Util.Data exposing (..)

import Api.Data
import List exposing (length)
import Model.Currency exposing (AssetIdentifier)


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
