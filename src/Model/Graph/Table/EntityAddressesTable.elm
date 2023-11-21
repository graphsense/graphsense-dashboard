module Model.Graph.Table.EntityAddressesTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table


titleAddress : String
titleAddress =
    "Address"


titleFirstUsage : String
titleFirstUsage =
    "First usage"


titleLastUsage : String
titleLastUsage =
    "Last usage"


titleFinalBalance : String
titleFinalBalance =
    "Balance"


titleTotalReceived : String
titleTotalReceived =
    "Total received"


filter : Table.Filter Api.Data.Address
filter =
    { search =
        \term a ->
            String.contains term a.address
    , filter = always True
    }
