module Init.Pathfinder.TxDetails exposing (..)

import Init.Graph.Table
import Model.Pathfinder.Table.IoTable as IoTable
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails as TxDetails
import Update.Graph.Table
import Util.Data exposing (negateTxValue)


init : Tx -> TxDetails.Model
init tx =
    let
        data =
            case tx.type_ of
                Tx.Utxo { raw } ->
                    (raw.inputs
                        |> Maybe.withDefault []
                        |> List.map negateTxValue
                    )
                        ++ (raw.outputs
                                |> Maybe.withDefault []
                           )

                Tx.Account _ ->
                    []
    in
    { ioTableOpen = True
    , table =
        Init.Graph.Table.initSorted False IoTable.titleValue
            |> Update.Graph.Table.setData IoTable.filter data
    , tx = tx
    }
