module Init.Graph.Table.TxUtxoTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.TxUtxoTable exposing (..)


init : Bool -> Table Api.Data.TxValue
init =
    columnTitleFromDirection >> Init.Graph.Table.init
