module Init.Pathfinder.TxDetails exposing (init, initSubTxTable)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Model.Pathfinder.Table.IoTable as IoTable
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails as TxDetails
import RemoteData
import Util.Data exposing (negateTxValue)
import Util.ThemedSelectBox as ThemedSelectBox


initSubTxTable : InfiniteTable.Model Api.Data.TxAccount
initSubTxTable =
    Table.initUnsorted
        |> InfiniteTable.init "subTxTable" 6


init : List String -> Tx -> TxDetails.Model
init assets tx =
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
    , baseTx = RemoteData.NotAsked
    , subTxsTable = initSubTxTable
    , subTxsTableOpen =
        tx
            |> Tx.getAccountTx
            |> Maybe.map
                (\t ->
                    (t.raw.isExternal
                        |> Maybe.withDefault False
                        |> not
                    )
                        || (t.value.value == 0)
                )
            |> Maybe.withDefault False
    , subTxsTableFilter =
        { includeZeroValueTxs = Just False
        , selectedAsset = Nothing
        , dateRangePicker = Nothing
        , direction = Nothing
        , assetSelectBox = ThemedSelectBox.init (Nothing :: List.map Just assets)
        , isSubTxsTableFilterDialogOpen = False
        }
    }
