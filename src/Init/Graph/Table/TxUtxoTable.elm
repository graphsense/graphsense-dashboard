module Init.Graph.Table.TxUtxoTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)
import Model.Graph.Table.TxUtxoTable exposing (..)


init : Bool -> Table Api.Data.TxValue
init =
    columnTitleFromDirection >> Table.init
