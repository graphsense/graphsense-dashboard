module View.Graph.Table.AddressNeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Address as A
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (none)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


tokenCurrencies : List String
tokenCurrencies =
    [ "usdt", "usdc", "weth" ]


columnTitleFromDirection : Bool -> String
columnTitleFromDirection isOutgoing =
    (if isOutgoing then
        "Outgoing"

     else
        "Incoming"
    )
        ++ " address"


init : Bool -> Table Api.Data.NeighborAddress
init =
    columnTitleFromDirection
        >> Init.Graph.Table.init filter


filter : String -> Api.Data.NeighborAddress -> Bool
filter f a =
    String.contains f a.address.address
        || (Maybe.map (List.any (String.contains f)) a.labels |> Maybe.withDefault True)


titleLabels : String
titleLabels =
    "Labels"


titleAddressBalance : String
titleAddressBalance =
    "Address balance"


titleAddressReceived : String
titleAddressReceived =
    "Address received"


titleNoTxs : String
titleNoTxs =
    "No. transactions"


titleEstimatedValue : String
titleEstimatedValue =
    "Estimated value"


config : View.Config -> Bool -> String -> Maybe AddressId -> (AddressId -> Bool -> A.Address -> Bool) -> Table.Config Api.Data.NeighborAddress Msg
config vc isOutgoing coinCode id neighborLayerHasAddress =
    Table.customConfig
        { toId = .address >> .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                (columnTitleFromDirection isOutgoing)
                (.address >> .address)
                (\data ->
                    [ id
                        |> Maybe.map
                            (\aid ->
                                T.tickIf vc
                                    (neighborLayerHasAddress aid isOutgoing)
                                    { currency = data.address.currency
                                    , address = data.address.address
                                    }
                            )
                        |> Maybe.withDefault none
                    , span
                        (id
                            |> Maybe.map
                                (\addressId ->
                                    [ UserClickedAddressInNeighborsTable addressId isOutgoing data
                                        |> onClick
                                    , css [ cursor pointer ]
                                    ]
                                )
                            |> Maybe.withDefault []
                        )
                        [ text data.address.address
                        ]
                    ]
                )
            , T.stringColumn vc titleLabels (.labels >> Maybe.withDefault [] >> reduceLabels)
            , T.intColumn vc titleNoTxs .noTxs
            ]
                ++ valueColumns vc
                    coinCode
                    (if coinCode == "eth" then
                        tokenCurrencies

                     else
                        []
                    )
                    { balance = .address >> .balance
                    , totalReceived = .address >> .totalReceived
                    , value = .value
                    }
        , customizations = customizations vc
        }


zero : Api.Data.Values
zero =
    { fiatValues = []
    , value = 0
    }


valueColumns :
    View.Config
    -> String
    -> List String
    ->
        { balance : Api.Data.NeighborAddress -> Api.Data.Values
        , totalReceived : Api.Data.NeighborAddress -> Api.Data.Values
        , value : Api.Data.NeighborAddress -> Api.Data.Values
        }
    -> List (Table.Column Api.Data.NeighborAddress Msg)
valueColumns vc coinCode tokens getValues =
    let
        getCurr c =
            Maybe.andThen (Dict.get c)
                >> Maybe.withDefault zero
    in
    (T.valueColumnWithoutCode vc (\_ -> coinCode) (titleAddressBalance ++ " " ++ String.toUpper coinCode) getValues.balance
        :: (tokens
                |> List.map
                    (\currency ->
                        T.valueColumnWithoutCode vc
                            (\_ -> currency)
                            (String.toUpper currency)
                            (.address >> .tokenBalances >> getCurr currency)
                    )
           )
    )
        ++ (T.valueColumnWithoutCode vc (\_ -> coinCode) (titleAddressReceived ++ " " ++ String.toUpper coinCode) getValues.totalReceived
                :: (tokens
                        |> List.map
                            (\currency ->
                                T.valueColumnWithoutCode vc
                                    (\_ -> currency)
                                    (String.toUpper currency ++ " ")
                                    (.address >> .totalTokensReceived >> getCurr currency)
                            )
                   )
           )
        ++ (T.valueColumnWithoutCode vc (\_ -> coinCode) (titleEstimatedValue ++ " " ++ String.toUpper coinCode) getValues.value
                :: (tokens
                        |> List.map
                            (\currency ->
                                T.valueColumnWithoutCode vc
                                    (\_ -> currency)
                                    (String.toUpper currency ++ "  ")
                                    (.tokenValues >> getCurr currency)
                            )
                   )
           )


reduceLabels : List String -> String
reduceLabels labels =
    let
        maxCount =
            30
    in
    labels
        |> List.foldl
            (\label acc ->
                if acc.i > 0 && acc.charCount > maxCount then
                    { acc
                        | output =
                            acc.output ++ " + " ++ (List.length labels - acc.i |> String.fromInt)
                    }

                else
                    { output =
                        acc.output
                            ++ (if acc.i > 0 then
                                    ", "

                                else
                                    ""
                               )
                            ++ (if String.length label < maxCount then
                                    label

                                else
                                    String.left maxCount label ++ "…"
                               )
                    , i = acc.i + 1
                    , charCount = acc.charCount + String.length label
                    }
            )
            { charCount = 0
            , i = 0
            , output = ""
            }
        |> .output


n s =
    ( s, [] )


prepareCSV : Bool -> String -> Api.Data.NeighborAddress -> List ( ( String, List String ), String )
prepareCSV isOutgoing coinCode row =
    let
        suffix =
            if coinCode == "eth" then
                "_eth"

            else
                ""
    in
    [ ( n <| "address", Util.Csv.string row.address.address )
    , ( n "labels", row.labels |> Maybe.withDefault [] |> String.join ", " |> Util.Csv.string )
    , ( n "no_txs", Util.Csv.int row.noTxs )
    ]
        ++ Util.Csv.values ("address_balance" ++ suffix) row.address.totalReceived
        ++ Util.Csv.values ("address_received" ++ suffix) row.address.balance
        ++ Util.Csv.values ("estimated_value" ++ suffix) row.value
        ++ (if coinCode == "eth" then
                prepareCsvTokens row

            else
                []
           )


prepareCsvTokens : Api.Data.NeighborAddress -> List ( ( String, List String ), String )
prepareCsvTokens row =
    tokenCurrencies
        |> List.map
            (\token ->
                Util.Csv.values ("address_balance_" ++ token)
                    (row.address.totalTokensReceived
                        |> Maybe.andThen (Dict.get token)
                        |> Maybe.withDefault zero
                    )
                    ++ Util.Csv.values ("address_received_" ++ token)
                        (row.address.tokenBalances
                            |> Maybe.andThen (Dict.get token)
                            |> Maybe.withDefault zero
                        )
                    ++ Util.Csv.values ("estimated_value_" ++ token)
                        (row.tokenValues
                            |> Maybe.andThen (Dict.get token)
                            |> Maybe.withDefault zero
                        )
            )
        |> List.concat
