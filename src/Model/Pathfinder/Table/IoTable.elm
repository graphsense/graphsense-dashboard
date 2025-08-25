module Model.Pathfinder.Table.IoTable exposing (filter, titleValue)

import Api.Data
import Components.Table as Table


filter : Table.Filter Api.Data.TxValue
filter =
    { search =
        \_ _ -> True
    , filter = always True
    }


titleValue : String
titleValue =
    "Value"
