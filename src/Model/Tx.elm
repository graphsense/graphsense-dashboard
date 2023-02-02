module Model.Tx exposing (..)


type alias Tx =
    { currency : String
    , txHash : String
    }


type alias TxAccount =
    { currency : String
    , txHash : String
    , tokenTxId : Maybe Int
    }
