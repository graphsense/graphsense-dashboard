module View.Graph.Table.AddressNeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Css.View as CssView
import Dict
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Address as A
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Table exposing (Table)
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (none)
import View.Button exposing (actorLink)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale
import View.Util exposing (copyableLongIdentifier)


columnTitleFromDirection : Bool -> String
columnTitleFromDirection isOutgoing =
    (if isOutgoing then
        "Outgoing"

     else
        "Incoming"
    )
        ++ " address"


init : Table Api.Data.NeighborAddress
init =
    Init.Graph.Table.initUnsorted filter


filter : String -> Api.Data.NeighborAddress -> Bool
filter f a =
    String.contains f a.address.address
        || (Maybe.map (List.any (String.contains f)) a.labels |> Maybe.withDefault True)


titleLabels : String
titleLabels =
    "Tags"


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


titleValue : String -> String
titleValue coinCode =
    if coinCode == "eth" then
        "Value"

    else
        titleEstimatedValue


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
                        [ copyableLongIdentifier vc data.address.address UserClickedCopyToClipboard
                        ]
                    ]
                )

            {- , T.stringColumn vc titleLabels (.labels >> Maybe.withDefault [] >> reduceLabels) -}
            , T.htmlColumn vc
                titleLabels
                (.labels >> Maybe.withDefault [] >> reduceLabels)
                (\data ->
                    case data.address.actors of
                        Just actors ->
                            actors
                                |> List.map
                                    (\ac -> actorLink vc ac.id ac.label)

                        Nothing ->
                            [ span [] [ text (data.labels |> Maybe.withDefault [] |> reduceLabels) ] ]
                )
            , T.intColumn vc titleNoTxs .noTxs
            ]
                ++ valueColumns vc
                    coinCode
                    (if coinCode == "eth" then
                        Locale.tokenCurrencies vc.locale

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

        ( suffix, valCol ) =
            if coinCode == "eth" then
                ( " " ++ String.toUpper coinCode, T.valueColumnWithoutCode )

            else
                ( "", T.valueColumn )
    in
    (valCol vc (\_ -> coinCode) (Locale.string vc.locale titleAddressBalance ++ suffix) getValues.balance
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
        ++ (valCol vc (\_ -> coinCode) (Locale.string vc.locale titleAddressReceived ++ suffix) getValues.totalReceived
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
        ++ (valCol vc (\_ -> coinCode) (Locale.string vc.locale (titleValue coinCode) ++ suffix) getValues.value
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
                        | j = acc.j + 1
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
                    , j = acc.j
                    , charCount = acc.charCount + String.length label
                    }
            )
            { charCount = 0
            , j = 0
            , i = 0
            , output = ""
            }
        |> (\{ output, j } ->
                if j > 0 then
                    output ++ " + " ++ String.fromInt j

                else
                    output
           )


n s =
    ( s, [] )


prepareCSV : Locale.Model -> Bool -> String -> Api.Data.NeighborAddress -> List ( ( String, List String ), String )
prepareCSV locale isOutgoing coinCode row =
    let
        suffix =
            if coinCode == "eth" then
                "_eth"

            else
                ""

        estimatedValueTitle =
            if coinCode == "eth" then
                "value"

            else
                "estimated_value"
    in
    [ ( n <| "address", Util.Csv.string row.address.address )
    , ( n "labels", row.labels |> Maybe.withDefault [] |> String.join ", " |> Util.Csv.string )
    , ( n "no_txs", Util.Csv.int row.noTxs )
    ]
        ++ Util.Csv.values ("address_balance" ++ suffix) row.address.totalReceived
        ++ Util.Csv.values ("address_received" ++ suffix) row.address.balance
        ++ Util.Csv.values (estimatedValueTitle ++ suffix) row.value
        ++ (if coinCode == "eth" then
                prepareCsvTokens locale row

            else
                []
           )


prepareCsvTokens : Locale.Model -> Api.Data.NeighborAddress -> List ( ( String, List String ), String )
prepareCsvTokens locale row =
    Locale.tokenCurrencies locale
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
                    ++ Util.Csv.values ("value_" ++ token)
                        (row.tokenValues
                            |> Maybe.andThen (Dict.get token)
                            |> Maybe.withDefault zero
                        )
            )
        |> List.concat
