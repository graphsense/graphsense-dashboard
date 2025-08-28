module Model.Pathfinder.TxDetails exposing (Model, transactionTableConfig, transactionTableFilter)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table exposing (Table)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg(..), TxDetailsMsg(..))
import RemoteData exposing (WebData)


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
                    , includeZeroValueSubTxs = m.includeZeroValueSubTxs
                    , pagesize = Just pagesize
                    , nextpage = nextpage
                    }
                |> ApiEffect
    }


transactionTableFilter : Table.Filter Api.Data.TxAccount
transactionTableFilter =
    { search =
        \_ _ -> True
    , filter = always True
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
    , includeZeroValueSubTxs : Bool
    }
