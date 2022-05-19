module Model.Graph.Browser exposing (..)

import Api.Data
import InfiniteList
import Model.Graph.Address exposing (Address)
import Model.Graph.Entity exposing (Entity)
import Time


type alias Model =
    { type_ : Type
    , table : TableType
    , visible : Bool
    , now : Time.Posix
    }


type Type
    = None
    | Address (Loadable String Address)
    | Entity (Loadable Int Entity)


type Loadable id thing
    = Loading String id
    | Loaded thing


type TableType
    = NoTable
    | AddressTable AddressTable


type AddressTable
    = AddressTagsTable (Table Api.Data.AddressTag)
    | AddressTxsTable (Table Api.Data.AddressTx)
    | AddressIncomingNeighborsTable (Table Api.Data.NeighborAddress)
    | AddressOutgoingNeighborsTable (Table Api.Data.NeighborAddress)


type alias Table a =
    { data : List a
    , loading : Bool
    , table : InfiniteList.Model
    , nextpage : Maybe String
    }
