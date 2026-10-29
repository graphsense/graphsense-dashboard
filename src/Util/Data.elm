module Util.Data exposing (absValues, addressCluster, isAccountLike, isEvmHexNetwork, isMixedCaseBech32, looksLikeTxHash, mulValues, negateTxValue, negateValues, normalizeIdCasing, normalizeIdentifier, normalizeTxIdentifier, parseMultiIdentifierInput, selfCluster, splitMultiIdentifierInput, subValues, sumValues, timestampToPosix)

import Api.Data
import Basics.Extra exposing (flip)
import Bitwise
import Round
import Time
import Util exposing (removeLeading0x)



-- import Util exposing (removeLeading0x)


supportedFiatCurrencies : List String
supportedFiatCurrencies =
    [ "eur", "usd" ]


timestampToPosix : Int -> Time.Posix
timestampToPosix =
    (*) 1000
        >> Time.millisToPosix


{-| The cluster an address belongs to. Prefers the fresh (incrementally
maintained) cluster id served by REST instances with fresh clustering
enabled; falls back to the legacy transform's cluster id otherwise.
-}
addressCluster : Api.Data.Address -> Int
addressCluster address =
    address.freshClusterId |> Maybe.withDefault address.cluster


{-| The cluster of an address on a network that serves no cluster data.

Account-model clusters are singletons — the server's own answer for e.g. eth
is `root_address` = the address, `no_addresses` = 1 and every stat mirrored
from the address — so the cluster can be derived locally instead of fetched.

-}
selfCluster : Api.Data.Address -> Api.Data.Cluster
selfCluster a =
    { actors = a.actors
    , balance = a.balance
    , bestAddressTag = Nothing
    , currency = a.currency
    , cluster = addressCluster a
    , firstTx = a.firstTx
    , inDegree = a.inDegree
    , lastTx = a.lastTx
    , noAddressTags = 0
    , noAddresses = 1
    , noIncomingTxs = a.noIncomingTxs
    , noOutgoingTxs = a.noOutgoingTxs
    , outDegree = a.outDegree
    , rootAddress = a.address
    , tokenBalances = a.tokenBalances
    , totalReceived = a.totalReceived
    , totalSpent = a.totalSpent
    , totalTokensReceived = a.totalTokensReceived
    , totalTokensSpent = a.totalTokensSpent
    }


isAccountLike : String -> Bool
isAccountLike network =
    let
        currl =
            String.toLower network
    in
    currl == "trx" || isEvmHexNetwork currl


{-| Networks whose addresses and tx hashes are 0x-prefixed, case-insensitive
hex (EVM). Tron is account-like but uses case-sensitive base58 identifiers,
so it is deliberately not in here.
-}
isEvmHexNetwork : String -> Bool
isEvmHexNetwork network =
    let
        currl =
            String.toLower network
    in
    List.member currl [ "eth", "bnb", "arb", "base", "opt", "pol", "avax", "gnosis", "robinhood" ]


negateValues : Api.Data.Values -> Api.Data.Values
negateValues x =
    let
        negateRate =
            \v -> { code = v.code, value = -v.value }
    in
    { value = -x.value, fiatValues = List.map negateRate x.fiatValues }


absValues : Api.Data.Values -> Api.Data.Values
absValues x =
    if x.value >= 0 then
        x

    else
        negateValues x


negateTxValue : Api.Data.TxValue -> Api.Data.TxValue
negateTxValue tv =
    { address = tv.address, index = tv.index, value = negateValues tv.value }


addValues : Api.Data.Values -> Api.Data.Values -> Api.Data.Values
addValues x y =
    let
        rates =
            List.map2 Tuple.pair x.fiatValues y.fiatValues

        fvalues =
            List.map (\( xf, yf ) -> { code = xf.code, value = xf.value + yf.value }) rates
    in
    { value = x.value + y.value, fiatValues = fvalues }


subValues : Api.Data.Values -> Api.Data.Values -> Api.Data.Values
subValues x y =
    let
        rates =
            List.map2 Tuple.pair x.fiatValues y.fiatValues

        fvalues =
            List.map (\( xf, yf ) -> { code = xf.code, value = xf.value - yf.value }) rates
    in
    { value = x.value - y.value, fiatValues = fvalues }


mulValues : Float -> Api.Data.Values -> Api.Data.Values
mulValues mul values =
    { value = toFloat values.value * mul |> round
    , fiatValues =
        List.map (\f -> { f | value = f.value * mul |> Round.roundNum 2 }) values.fiatValues
    }


sumValues : List Api.Data.Values -> Api.Data.Values
sumValues values =
    List.foldl addValues valuesZero values


valuesZero : Api.Data.Values
valuesZero =
    { value = 0, fiatValues = supportedFiatCurrencies |> List.map (\c -> { code = c, value = 0.0 }) }


ensure0x : String -> String
ensure0x s =
    if String.startsWith "0x" s then
        s

    else
        "0x" ++ s


{-| Lowercase the hex part of an address or tx identifier on networks
whose identifiers are case-insensitive hex (eth, bnb). Only the segment
before the first "\_" is lowercased: sub-tx markers like "\_T1"/"\_I1"
are case-sensitive and must be preserved. Other networks (btc, trx)
use case-sensitive encodings and keep their case, except an all-uppercase
bech32 address (BIP173, QR codes), which becomes the lowercase form the API
serves, as in `normalizeIdentifier`.
-}
normalizeIdCasing : String -> String -> String
normalizeIdCasing network identifier =
    if isEvmHexNetwork network then
        case String.split "_" identifier of
            hex :: suffix ->
                String.join "_" (String.toLower hex :: suffix)

            [] ->
                identifier

    else
        lowercaseUppercaseBech32 network identifier


{-| Canonical form of a TX identifier for graph node ids and API requests:
served identifiers carry no "0x" and case-SENSITIVE sub-tx markers
("\_T1"/"\_I1"), so only the hex part before the first "\_" may be
lowercased and a leading "0x" is stripped. Never use `normalizeIdentifier`
(the ADDRESS normalizer) on a tx: its whole-string toLower turns "\_T2"
into the invalid "\_t2" and its ensure0x mismatches served tx ids.
-}
normalizeTxIdentifier : String -> String -> String
normalizeTxIdentifier net txId =
    if isEvmHexNetwork net then
        normalizeIdCasing net (removeLeading0x (String.trim txId))

    else
        txId


normalizeIdentifier : String -> String -> String
normalizeIdentifier net address =
    String.trim address
        |> (if isEvmHexNetwork net then
                String.toLower >> ensure0x

            else
                lowercaseUppercaseBech32 net
           )


{-| BIP173 allows a bech32 address in all uppercase as well (QR codes use
it). The API accepts that form but serves the lowercase one, so graph node
ids must use the lowercase one too, or one address becomes two nodes. Mixed
case is invalid bech32 and stays as it is. The prefixes are the ones the API
accepts in uppercase.
-}
lowercaseUppercaseBech32 : String -> String -> String
lowercaseUppercaseBech32 net address =
    let
        prefixes =
            case String.toLower net of
                "btc" ->
                    [ "BC1", "TB1", "BCRT1" ]

                "ltc" ->
                    [ "LTC1", "TLTC1", "RLTC1" ]

                _ ->
                    []
    in
    if address == String.toUpper address && List.any (\p -> String.startsWith p address) prefixes then
        String.toLower address

    else
        address


{-| A valid bech32 address in mixed upper and lower case: only the case is
wrong (BIP173 allows all lowercase or all uppercase, never both). The same
rule as graphsenselib's `bech32_mixed_case`: a base58 address that starts
like bech32 by chance (LTC: "L" + "TC1…") is never one, because in lowercase
its checksum fails.
-}
isMixedCaseBech32 : String -> Bool
isMixedCaseBech32 address =
    let
        lower =
            String.toLower address
    in
    (address /= lower)
        && (address /= String.toUpper address)
        && List.any (\hrp -> String.startsWith (hrp ++ "1") lower) [ "bc", "tb", "bcrt", "ltc", "tltc", "rltc" ]
        && bech32ChecksumOk lower


{-| Only the checksum of a lowercase bech32 string: either constant counts
(bech32 or bech32m), like graphsenselib's `_bech32_checksum_ok`.
-}
bech32ChecksumOk : String -> Bool
bech32ChecksumOk s =
    case String.indexes "1" s |> List.reverse |> List.head of
        Just pos ->
            let
                dataPart =
                    String.dropLeft (pos + 1) s

                data =
                    String.toList dataPart
                        |> List.filterMap (\c -> String.indexes (String.fromChar c) "qpzry9x8gf2tvdw0s3jn54khce6mua7l" |> List.head)
            in
            (pos >= 1)
                && (pos + 7 <= String.length s)
                && (List.length data == String.length dataPart)
                && List.member (bech32Polymod (bech32HrpExpand (String.left pos s) ++ data)) [ 1, 0x2BC830A3 ]

        Nothing ->
            False


bech32HrpExpand : String -> List Int
bech32HrpExpand hrp =
    let
        codes =
            String.toList hrp |> List.map Char.toCode
    in
    List.map (Bitwise.shiftRightBy 5) codes ++ [ 0 ] ++ List.map (Bitwise.and 31) codes


bech32Polymod : List Int -> Int
bech32Polymod =
    List.foldl
        (\value chk ->
            let
                top =
                    Bitwise.shiftRightZfBy 25 chk
            in
            List.foldl
                (\( i, generator ) acc ->
                    if Bitwise.and (Bitwise.shiftRightZfBy i top) 1 == 1 then
                        Bitwise.xor acc generator

                    else
                        acc
                )
                (Bitwise.xor (Bitwise.shiftLeftBy 5 (Bitwise.and chk 0x01FFFFFF)) value)
                (List.indexedMap Tuple.pair [ 0x3B6A57B2, 0x26508E6D, 0x1EA119FA, 0x3D4233DD, 0x2A1462B3 ])
        )
        1


{-| Whether an identifier has the shape of a transaction hash on any
supported network: 64 hex digits, with or without a leading 0x. Everything
else is taken for an address.
-}
looksLikeTxHash : String -> Bool
looksLikeTxHash identifier =
    let
        hash =
            removeLeading0x identifier
    in
    String.length hash == 64 && String.all Char.isHexDigit hash


{-| The tokens of a multi-identifier paste that are long enough to be an
address or a transaction hash. See `splitMultiIdentifierInput` for the ones
that are dropped here.
-}
parseMultiIdentifierInput : String -> List String
parseMultiIdentifierInput input =
    splitMultiIdentifierInput input
        |> List.filter (\s -> String.length s >= 15)


{-| Every non-empty token of a multi-identifier paste, however short. Empty
unless the input contains a separator at all, so a single identifier is not a
paste of one.
-}
splitMultiIdentifierInput : String -> List String
splitMultiIdentifierInput input =
    let
        spliters =
            [ ",", " ", "\n", "\t", ";" ]

        isMultiInput =
            spliters
                |> List.any (flip String.contains input)

        inputList =
            if isMultiInput then
                spliters
                    |> List.foldl
                        (\sp acc ->
                            acc
                                |> List.concatMap (String.split sp)
                        )
                        [ input ]

            else
                []
    in
    inputList
        |> List.map (String.trim >> removeLeading0x)
        |> List.filter (not << String.isEmpty)
