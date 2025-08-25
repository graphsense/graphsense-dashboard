module Init.Graph.Table.TxsUtxoTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)


init : Table Api.Data.TxUtxo
init =
    Table.initUnsorted
