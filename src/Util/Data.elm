module Util.Data exposing (absValues, addValues, averageFiatValue, isAccountLike, negateTxValue, negateValues, normalizeIdentifier, parseMultiIdentifierInput, sumValues, timestampToPosix, valuesZero)

import Api.Data
import Basics.Extra exposing (flip)
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


averageFiatValue : Api.Data.Values -> Float
averageFiatValue { fiatValues } =
    (fiatValues
        |> List.map .value
        |> List.sum
    )
        / (toFloat <| List.length fiatValues)


isAccountLike : String -> Bool
isAccountLike network =
    let
        currl =
            String.toLower network
    in
    currl == "eth" || currl == "trx"


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


normalizeIdentifier : String -> String -> String
normalizeIdentifier net address =
    String.trim address
        |> (if net == "eth" then
                String.toLower >> ensure0x

            else
                identity
           )


parseMultiIdentifierInput : String -> List String
parseMultiIdentifierInput input =
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
        |> List.filter (\s -> String.length s >= 15)



-- |> removeLeading0x
