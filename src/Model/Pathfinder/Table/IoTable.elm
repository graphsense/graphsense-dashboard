module Model.Pathfinder.Table.IoTable exposing (..)

import Api.Data
import Model.Graph.Table as Table

filter : Table.Filter Api.Data.TxValue
filter =
    { search =
        \term a -> True
    , filter = always True
    }

titleValue =
    "Value"
