module Model.Graph.Table.AllAssetsTable exposing (..)

import Api.Data
import Model.Graph.Table as Table
import Tuple exposing (first)
import Model.Currency exposing (AssetIdentifier)


filter : Table.Filter ( AssetIdentifier, Api.Data.Values )
filter =
    { search =
        \term a -> String.contains term (first a).network || String.contains term (first a).asset
    , filter = always True
    }
