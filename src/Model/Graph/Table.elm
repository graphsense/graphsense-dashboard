module Model.Graph.Table exposing (ActorTable(..), AddressTable(..), AddresslinkTable(..), AllAssetsTable, BlockTable(..), EntityTable(..), TxAccountTable(..), TxUtxoTable(..), titleAddress, titleCurrency, titleHeight, titleLabel, titleTimestamp, titleTx, titleValue)

import Api.Data
import Components.Table exposing (Table)
import Model.Currency exposing (AssetIdentifier)


titleTx : String
titleTx =
    "Transaction"


titleValue : String
titleValue =
    "Value"


titleHeight : String
titleHeight =
    "Height"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


titleAddress : String
titleAddress =
    "Address"


titleCurrency : String
titleCurrency =
    "Currency"


titleLabel : String
titleLabel =
    "Label"


type AddressTable
    = AddressTagsTable (Table Api.Data.AddressTag)
    | AddressTxsUtxoTable (Table Api.Data.AddressTxUtxo)
    | AddressTxsAccountTable (Table Api.Data.TxAccount)
    | AddressIncomingNeighborsTable (Table Api.Data.NeighborAddress)
    | AddressOutgoingNeighborsTable (Table Api.Data.NeighborAddress)
    | AddressTotalReceivedAllAssetsTable AllAssetsTable
    | AddressFinalBalanceAllAssetsTable AllAssetsTable


type EntityTable
    = EntityTagsTable (Table Api.Data.AddressTag)
    | EntityTxsUtxoTable (Table Api.Data.AddressTxUtxo)
    | EntityTxsAccountTable (Table Api.Data.TxAccount)
    | EntityIncomingNeighborsTable (Table Api.Data.NeighborEntity)
    | EntityOutgoingNeighborsTable (Table Api.Data.NeighborEntity)
    | EntityAddressesTable (Table Api.Data.Address)
    | EntityTotalReceivedAllAssetsTable AllAssetsTable
    | EntityFinalBalanceAllAssetsTable AllAssetsTable


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
    | AddresslinkAllAssetsTable AllAssetsTable


type alias AllAssetsTable =
    Table ( AssetIdentifier, Api.Data.Values )
