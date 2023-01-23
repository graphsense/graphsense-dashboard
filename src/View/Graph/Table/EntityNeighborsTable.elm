module View.Graph.Table.EntityNeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Entity as E
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (none)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Graph.Table.AddressNeighborsTable exposing (reduceLabels)
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
        ++ " entity"


init : Bool -> Table Api.Data.NeighborEntity
init =
    columnTitleFromDirection >> Init.Graph.Table.init filter


filter : String -> Api.Data.NeighborEntity -> Bool
filter f a =
    String.contains f (String.fromInt a.entity.entity)
        || (Maybe.map (List.any (String.contains f)) a.labels |> Maybe.withDefault True)


titleLabels : String
titleLabels =
    "Labels"


titleEntityBalance : String
titleEntityBalance =
    "Entity balance"


titleEntityReceived : String
titleEntityReceived =
    "Entity received"


titleNoAddresses : String
titleNoAddresses =
    "No. addresses"


titleNoTxs : String
titleNoTxs =
    "No. transactions"


titleEstimatedValue : String
titleEstimatedValue =
    "Estimated value"


config : View.Config -> Bool -> String -> Maybe EntityId -> (EntityId -> Bool -> E.Entity -> Bool) -> Table.Config Api.Data.NeighborEntity Msg
config vc isOutgoing coinCode id neighborLayerHasEntity =
    Table.customConfig
        { toId = .entity >> .entity >> String.fromInt
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                (columnTitleFromDirection isOutgoing)
                (.entity >> .entity >> String.fromInt)
                (\data ->
                    [ id
                        |> Maybe.map
                            (\eid ->
                                T.tickIf vc
                                    (neighborLayerHasEntity eid isOutgoing)
                                    { currency = data.entity.currency
                                    , entity = data.entity.entity
                                    }
                            )
                        |> Maybe.withDefault none
                    , span
                        (id
                            |> Maybe.map
                                (\entityId ->
                                    [ UserClickedEntityInNeighborsTable entityId isOutgoing data
                                        |> onClick
                                    , css [ cursor pointer ]
                                    ]
                                )
                            |> Maybe.withDefault []
                        )
                        [ data.entity.entity
                            |> String.fromInt
                            |> text
                        ]
                    ]
                )
            , T.stringColumn vc titleLabels (.labels >> Maybe.withDefault [] >> reduceLabels)
            , T.intColumn vc titleNoAddresses (.entity >> .noAddresses)
            , T.intColumn vc titleNoTxs .noTxs
            ]
                ++ valueColumns vc
                    coinCode
                    (if coinCode == "eth" then
                        [ "usdt", "usdc", "weth" ]

                     else
                        []
                    )
                    { balance = .entity >> .balance
                    , totalReceived = .entity >> .totalReceived
                    , value = .value
                    }
        , customizations = customizations vc
        }


valueColumns :
    View.Config
    -> String
    -> List String
    ->
        { balance : Api.Data.NeighborEntity -> Api.Data.Values
        , totalReceived : Api.Data.NeighborEntity -> Api.Data.Values
        , value : Api.Data.NeighborEntity -> Api.Data.Values
        }
    -> List (Table.Column Api.Data.NeighborEntity Msg)
valueColumns vc coinCode tokens getValues =
    let
        getCurr c =
            Maybe.andThen (Dict.get c)
                >> Maybe.withDefault zero
    in
    (T.valueColumnWithoutCode vc (\_ -> coinCode) (titleEntityBalance ++ " " ++ String.toUpper coinCode) getValues.balance
        :: (tokens
                |> List.map
                    (\currency ->
                        T.valueColumnWithoutCode vc
                            (\_ -> currency)
                            (String.toUpper currency)
                            (.entity >> .tokenBalances >> getCurr currency)
                    )
           )
    )
        ++ (T.valueColumnWithoutCode vc (\_ -> coinCode) (titleEntityReceived ++ " " ++ String.toUpper coinCode) getValues.totalReceived
                :: (tokens
                        |> List.map
                            (\currency ->
                                T.valueColumnWithoutCode vc
                                    (\_ -> currency)
                                    (String.toUpper currency ++ " ")
                                    (.entity >> .totalTokensReceived >> getCurr currency)
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


zero : Api.Data.Values
zero =
    { fiatValues = []
    , value = 0
    }


n s =
    ( s, [] )


prepareCSV : Bool -> String -> Api.Data.NeighborEntity -> List ( ( String, List String ), String )
prepareCSV isOutgoing coinCode row =
    let
        suffix =
            if coinCode == "eth" then
                "_eth"

            else
                ""
    in
    [ ( n <| "entity", Util.Csv.int row.entity.entity )
    , ( n "labels", row.labels |> Maybe.withDefault [] |> String.join ", " |> Util.Csv.string )
    , ( n "no_txs", Util.Csv.int row.noTxs )
    , ( n "no_addresses", Util.Csv.int row.entity.noAddresses )
    ]
        ++ Util.Csv.values ("entity_received" ++ suffix) row.entity.totalReceived
        ++ Util.Csv.values ("entity_balance" ++ suffix) row.entity.balance
        ++ Util.Csv.values ("estimated_value" ++ suffix) row.value
        ++ (if coinCode == "eth" then
                prepareCsvTokens row

            else
                []
           )


prepareCsvTokens : Api.Data.NeighborEntity -> List ( ( String, List String ), String )
prepareCsvTokens row =
    tokenCurrencies
        |> List.map
            (\token ->
                Util.Csv.values ("entity_balance_" ++ token)
                    (row.entity.totalTokensReceived
                        |> Maybe.andThen (Dict.get token)
                        |> Maybe.withDefault zero
                    )
                    ++ Util.Csv.values ("entity_received_" ++ token)
                        (row.entity.tokenBalances
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
