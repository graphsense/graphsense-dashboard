module Init.Pathfinder.TxDetails exposing (init)

import Components.Table as Table
import Model.Pathfinder.Table.IoTable as IoTable
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails as TxDetails
import Util.Data exposing (negateTxValue)


init : Tx -> TxDetails.Model
init tx =
    let
        ( inputs, outputs ) =
            case tx.type_ of
                Tx.Utxo { raw } ->
                    ( raw.inputs
                        |> Maybe.withDefault []
                        |> List.map negateTxValue
                    , raw.outputs
                        |> Maybe.withDefault []
                    )

                Tx.Account _ ->
                    ( [], [] )
    in
    { inputsTableOpen = False
    , outputsTableOpen = False
    , inputsTable =
        Table.initSorted False IoTable.titleValue
            |> Table.setData IoTable.filter inputs
    , outputsTable =
        Table.initSorted False IoTable.titleValue
            |> Table.setData IoTable.filter outputs
    , tx = tx
    }
