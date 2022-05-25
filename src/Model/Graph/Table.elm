module Model.Graph.Table exposing (..)

import Api.Data
import Table


type AddressTable
    = AddressTagsTable (Table Api.Data.AddressTag)
    | AddressTxsTable (Table Api.Data.AddressTxUtxo)
    | AddressIncomingNeighborsTable (Table Api.Data.NeighborAddress)
    | AddressOutgoingNeighborsTable (Table Api.Data.NeighborAddress)


type EntityTable
    = EntityTagsTable (Table Api.Data.AddressTag)
    | EntityTxsTable (Table Api.Data.AddressTx)
    | EntityIncomingNeighborsTable (Table Api.Data.NeighborEntity)
    | EntityOutgoingNeighborsTable (Table Api.Data.NeighborEntity)
    | EntityAddressesTable (Table Api.Data.Address)


type alias Table a =
    { data : List a
    , loading : Bool
    , state : Table.State
    , nextpage : Maybe String
    }
