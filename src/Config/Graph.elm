module Config.Graph exposing (AddressLabelType(..), Config, TxLabelType(..))


type AddressLabelType
    = ID
    | Balance
    | TotalReceived
    | Tag


type TxLabelType
    = NoTxs
    | Value


type alias Config =
    { addressLabelType : AddressLabelType
    , txLabelType : TxLabelType
    , maxLettersPerLabelRow : Int
    , highlighter : Bool
    , showEntityShadowLinks : Bool
    , showAddressShadowLinks : Bool
    , showZeroTransactions : Bool
    }
