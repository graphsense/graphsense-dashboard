module View.Graph.Table.AddressNeighborsTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Css.Table exposing (styles)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Address as A
import Model.Currency exposing (AssetIdentifier, assetFromBase)
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.AddressNeighborsTable exposing (..)
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.Data as Data
import Util.View exposing (copyableLongIdentifier, none)
import View.Button exposing (actorLink)
import View.Graph.Table as T exposing (customizations)
import View.Locale as Locale


columnTitleFromDirection : Bool -> String
columnTitleFromDirection isOutgoing =
    (if isOutgoing then
        "Outgoing"

     else
        "Incoming"
    )
        ++ " address"


config : View.Config -> Bool -> String -> Maybe AddressId -> (AddressId -> Bool -> A.Address -> Bool) -> Table.Config Api.Data.NeighborAddress Msg
config vc isOutgoing coinCode id neighborLayerHasAddress =
    Table.customConfig
        { toId = .address >> .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
                (columnTitleFromDirection isOutgoing)
                (.address >> .address)
                (\data ->
                    [ id
                        |> Maybe.map
                            (\aid ->
                                T.tickIf styles
                                    vc
                                    (neighborLayerHasAddress aid isOutgoing)
                                    { currency = data.address.currency
                                    , address = data.address.address
                                    }
                            )
                        |> Maybe.withDefault none
                    , copyableLongIdentifier vc
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
                        data.address.address
                    ]
                )

            {- , T.stringColumn vc titleLabels (.labels >> Maybe.withDefault [] >> reduceLabels) -}
            , T.htmlColumn styles
                vc
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
            , T.intColumn styles vc titleNoTxs .noTxs
            ]
                ++ valueColumns vc
                    (assetFromBase coinCode)
                    (if Data.isAccountLike coinCode then
                        Locale.tokenCurrencies coinCode vc.locale

                     else
                        []
                    )
                    { balance = .address >> .balance
                    , totalReceived = .address >> .totalReceived
                    , value = .value
                    }
        , customizations = customizations styles vc
        }


zero : Api.Data.Values
zero =
    { fiatValues = []
    , value = 0
    }


valueColumns :
    View.Config
    -> AssetIdentifier
    -> List String
    ->
        { balance : Api.Data.NeighborAddress -> Api.Data.Values
        , totalReceived : Api.Data.NeighborAddress -> Api.Data.Values
        , value : Api.Data.NeighborAddress -> Api.Data.Values
        }
    -> List (Table.Column Api.Data.NeighborAddress Msg)
valueColumns vc coinCode tokens getValues =
    [ T.valueAndTokensColumnWithOptions styles
        True
        vc
        (\_ -> coinCode.network)
        (Locale.string vc.locale titleAddressBalance)
        (.address >> .balance)
        (.address >> .tokenBalances)
    , T.valueAndTokensColumnWithOptions styles
        True
        vc
        (\_ -> coinCode.network)
        (Locale.string vc.locale titleAddressReceived)
        (.address >> .totalReceived)
        (.address >> .totalTokensReceived)
    , T.valueAndTokensColumnWithOptions styles
        True
        vc
        (\_ -> coinCode.network)
        (Locale.string vc.locale (titleValue coinCode.network))
        .value
        .tokenValues
    ]


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


n : x -> ( x, List y )
n s =
    ( s, [] )


prepareCSV : Locale.Model -> Bool -> String -> Api.Data.NeighborAddress -> List ( ( String, List String ), String )
prepareCSV locale isOutgoing network row =
    let
        suffix =
            if Data.isAccountLike network then
                "_eth"

            else
                ""

        estimatedValueTitle =
            if Data.isAccountLike network then
                "value"

            else
                "estimated_value"
    in
    [ ( n <| "address", Util.Csv.string row.address.address )
    , ( n "labels", row.labels |> Maybe.withDefault [] |> String.join ", " |> Util.Csv.string )
    , ( n "no_txs", Util.Csv.int row.noTxs )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat ("address_balance" ++ suffix) row.address.totalReceived locale network
        ++ Util.Csv.valuesWithBaseCurrencyFloat ("address_received" ++ suffix) row.address.balance locale network
        ++ Util.Csv.valuesWithBaseCurrencyFloat (estimatedValueTitle ++ suffix) row.value locale network
        ++ (if Data.isAccountLike network then
                prepareCsvTokens locale network row

            else
                []
           )


prepareCsvTokens : Locale.Model -> String -> Api.Data.NeighborAddress -> List ( ( String, List String ), String )
prepareCsvTokens locale coinCode row =
    Locale.tokenCurrencies coinCode locale
        |> List.concatMap
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
