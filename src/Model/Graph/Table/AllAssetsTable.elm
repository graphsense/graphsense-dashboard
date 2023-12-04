module Model.Graph.Table.AllAssetsTable exposing (..)

import Api.Data
import Model.Graph.Table as Table
import Tuple exposing (first)


filter : Table.Filter ( String, Api.Data.Values )
filter =
    { search =
        \term a -> String.contains term (first a)
    , filter = always True
    }
