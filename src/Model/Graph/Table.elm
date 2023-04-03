module Model.Graph.Table exposing (..)

import Api.Data
import Table


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


type ActorTable
    = ActorTagsTable (Table Api.Data.AddressTag)
    | ActorOtherLinksTable (Table String)


type TxUtxoTable
    = TxUtxoInputsTable (Table Api.Data.TxValue)
    | TxUtxoOutputsTable (Table Api.Data.TxValue)


type TxAccountTable
    = TokenTxsTable (Table Api.Data.TxAccount)


type BlockTable
    = BlockTxsUtxoTable (Table Api.Data.TxUtxo)
    | BlockTxsAccountTable (Table Api.Data.TxAccount)


type AddresslinkTable
    = AddresslinkTxsUtxoTable (Table Api.Data.LinkUtxo)
    | AddresslinkTxsAccountTable (Table Api.Data.TxAccount)


type alias Table a =
    { data : List a
    , filtered : List a
    , loading : Bool
    , state : Table.State
    , nextpage : Maybe String
    , filter : Maybe String
    , filterFunction : String -> a -> Bool
    }
