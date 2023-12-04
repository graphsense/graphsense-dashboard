module Model.Currency exposing (..)

import Api.Data
import List.Extra


type Currency
    = Coin
    | Fiat String
