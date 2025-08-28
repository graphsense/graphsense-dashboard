module Model.Pathfinder.TxDetails exposing (Model, SubTxTableFilter, transactionTableConfig, transactionTableFilter)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table exposing (Table)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg(..), TxDetailsMsg(..))
import RemoteData exposing (WebData)
import Util.ThemedSelectBox as ThemedSelectBox


transactionTableConfig : Model -> InfiniteTable.Config Effect
transactionTableConfig m =
    let
        baseTxHash =
            m.tx |> Tx.getRawBaseTxHashForTx

        currency =
            m.tx |> Tx.getNetwork
    in
    { fetch =
        \pagesize nextpage ->
            (BrowserGotTxFlows >> TxDetailsMsg)
                |> Api.ListTxFlowsEffect
                    { currency = currency
                    , txHash = baseTxHash
                    , includeZeroValueSubTxs = m.subTxsTableFilter.includeZeroValueTxs |> Maybe.withDefault False
                    , pagesize = Just pagesize
                    , nextpage = nextpage
                    }
                |> ApiEffect
    }


transactionTableFilter : Maybe String -> Table.Filter Api.Data.TxAccount
transactionTableFilter asset =
    let
        filter x =
            case asset of
                Just a ->
                    (x.currency |> String.toUpper) == (a |> String.toUpper)

                Nothing ->
                    True
    in
    { search =
        \_ _ -> True
    , filter = filter
    }


type alias SubTxTableFilter =
    { includeZeroValueTxs : Maybe Bool
    , isSubTxsTableFilterDialogOpen : Bool
    , selectedAsset : Maybe String
    , dateRangePicker : Maybe (DateRangePicker.Model TxDetailsMsg)
    , direction : Maybe Direction
    , assetSelectBox : ThemedSelectBox.Model (Maybe String)
    }


type alias Model =
    { inputsTableOpen : Bool
    , outputsTableOpen : Bool
    , inputsTable : Table Api.Data.TxValue
    , outputsTable : Table Api.Data.TxValue
    , tx : Tx
    , subTxsTableOpen : Bool
    , baseTx : WebData Api.Data.TxAccount
    , subTxsTable : InfiniteTable.Model Api.Data.TxAccount
    , subTxsTableFilter : SubTxTableFilter
    }
