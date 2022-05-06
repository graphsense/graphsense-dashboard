module Store.Msg exposing (..)

import Api.Data


type Msg
    = BrowserGotAddress Api.Data.Address
