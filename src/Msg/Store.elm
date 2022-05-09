module Msg.Store exposing (..)

import Api.Data


type Msg
    = BrowserGotAddress Api.Data.Address
    | BrowserGotEntity String Api.Data.Entity
    | BrowserGotEntityForAddress String Api.Data.Entity
