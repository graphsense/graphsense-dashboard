module Model.Currency exposing (..)

import Api.Data
import List.Extra


type Currency
    = Coin
    | Fiat String


valuesToFloat : Currency -> Api.Data.Values -> Maybe Float
valuesToFloat currency values =
    case currency of
        Coin ->
            values.value
                |> toFloat
                |> Just

        Fiat curr ->
            List.Extra.find (.code >> (==) curr) values.fiatValues
                |> Maybe.map .value
