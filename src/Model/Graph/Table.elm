module Model.Graph.Table exposing (..)

import Api.Data
import Table
import Util.InfiniteScroll as InfiniteScroll


type AddressTable
    = AddressTagsTable (Table Api.Data.AddressTag)
    | AddressTxsUtxoTable (Table Api.Data.AddressTxUtxo)
    | AddressTxsAccountTable (Table Api.Data.TxAccount)
    | AddressIncomingNeighborsTable (Table Api.Data.NeighborAddress)
    | AddressOutgoingNeighborsTable (Table Api.Data.NeighborAddress)


type EntityTable
    = EntityTagsTable (Table Api.Data.AddressTag)
    | EntityTxsUtxoTable (Table Api.Data.AddressTxUtxo)
    | EntityTxsAccountTable (Table Api.Data.TxAccount)
    | EntityIncomingNeighborsTable (Table Api.Data.NeighborEntity)
    | EntityOutgoingNeighborsTable (Table Api.Data.NeighborEntity)
    | EntityAddressesTable (Table Api.Data.Address)


type TxUtxoTable
    = TxUtxoInputsTable (Table Api.Data.TxValue)
    | TxUtxoOutputsTable (Table Api.Data.TxValue)


type BlockTable
    = BlockTxsUtxoTable (Table Api.Data.TxUtxo)
    | BlockTxsAccountTable (Table Api.Data.TxAccount)


type alias Table a =
    { data : List a
    , loading : Bool
    , state : Table.State
    , nextpage : Maybe String
    , infiniteScroll : InfiniteScroll.Model
    }
