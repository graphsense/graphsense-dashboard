module Model.Tx exposing (AccountTxType(..), Tx, TxAccount, getTxHash, parseTxIdentifier, txTypeToLabel)

import Parser exposing ((|.), (|=), Parser, backtrackable, end, int, keyword, oneOf, run, succeed, symbol, variable)
import Set


type alias Tx =
    { currency : String
    , txHash : String
    }


type alias TxAccount =
    { currency : String
    , txHash : String
    , tokenTxId : Maybe Int
    }


hexStringWithPrefix : Parser String
hexStringWithPrefix =
    oneOf [ succeed identity |. keyword "0x" |= hexString, hexString ]


hexString : Parser String
hexString =
    variable
        { start = \_ -> True
        , inner = \c -> Char.isHexDigit c
        , reserved = Set.empty
        }


type AccountTxType
    = External String
    | Internal String Int
    | Token String Int


parseTxIdentifier_ : Parser AccountTxType
parseTxIdentifier_ =
    oneOf
        [ succeed Token |= hexStringWithPrefix |. symbol "_" |. symbol "T" |= int |. end |> backtrackable
        , succeed Internal |= hexStringWithPrefix |. symbol "_" |. symbol "I" |= int |. end |> backtrackable
        , succeed External |= hexStringWithPrefix |. end
        ]


parseTxIdentifier : String -> Maybe AccountTxType
parseTxIdentifier =
    run parseTxIdentifier_ >> Result.toMaybe


getTxHash : AccountTxType -> String
getTxHash ac =
    case ac of
        Internal x _ ->
            x

        External x ->
            x

        Token x _ ->
            x


txTypeToLabel : AccountTxType -> String
txTypeToLabel x =
    case x of
        External _ ->
            "Transaction"

        Internal _ _ ->
            "Sub-Transaction"

        Token _ _ ->
            "Token-Transaction"
