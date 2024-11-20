module Model.Graph.Table.AllAssetsTable exposing (filter)

import Api.Data
import Model.Currency exposing (AssetIdentifier)
import Model.Graph.Table as Table
import Tuple exposing (first)


filter : Table.Filter ( AssetIdentifier, Api.Data.Values )
filter =
    { search =
        \term a -> String.contains term (first a).asset
    , filter = always True
    }
