module View.Graph.Table.EntityNeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Maybe.Extra
import Model.Entity as E
import Model.Graph.Entity
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.AddressNeighborsTable exposing (titleLabels, titleNoTxs, titleValue)
import Model.Graph.Table.EntityNeighborsTable exposing (..)
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (none)
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
        ++ " entity"


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

            {- , T.stringColumn vc titleLabels (.labels >> Maybe.withDefault [] >> reduceLabels) -}
            , T.htmlColumn vc
                titleLabels
                (\data ->
                    data.entity
                        |> Model.Graph.Entity.getBestActorApi
                        |> Maybe.map .id
                        |> Maybe.Extra.orElseLazy
                            (\_ ->
                                data.entity.bestAddressTag
                                    |> Maybe.map .label
                            )
                        |> Maybe.withDefault ""
                )
                (\data ->
                    case data.entity |> Model.Graph.Entity.getBestActorApi of
                        Just actor ->
                            [ actorLink vc actor.id actor.label ]

                        Nothing ->
                            [ span []
                                [ text
                                    (data.entity.bestAddressTag
                                        |> Maybe.map .label
                                        |> Maybe.withDefault ""
                                    )
                                ]
                            ]
                )
            , T.intColumn vc titleNoAddresses (.entity >> .noAddresses)
            , T.intColumn vc titleNoTxs .noTxs
            ]
                ++ valueColumns vc
                    coinCode
                    (if coinCode == "eth" then
                        Locale.tokenCurrencies vc.locale

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
    [ T.valueAndTokensColumnWithOptions True
        vc
        (\_ -> coinCode)
        (Locale.string vc.locale titleEntityBalance)
        (.entity >> .balance)
        (.entity >> .tokenBalances)
    , T.valueAndTokensColumnWithOptions True
        vc
        (\_ -> coinCode)
        (Locale.string vc.locale titleEntityReceived)
        (.entity >> .totalReceived)
        (.entity >> .totalTokensReceived)
    , T.valueAndTokensColumnWithOptions True
        vc
        (\_ -> coinCode)
        (Locale.string vc.locale (titleValue coinCode))
        .value
        .tokenValues
    ]


zero : Api.Data.Values
zero =
    { fiatValues = []
    , value = 0
    }


n s =
    ( s, [] )


prepareCSV : Locale.Model -> Bool -> String -> Api.Data.NeighborEntity -> List ( ( String, List String ), String )
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
    [ ( n <| "entity", Util.Csv.int row.entity.entity )
    , ( n "labels", row.labels |> Maybe.withDefault [] |> String.join ", " |> Util.Csv.string )
    , ( n "no_txs", Util.Csv.int row.noTxs )
    , ( n "no_addresses", Util.Csv.int row.entity.noAddresses )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat ("entity_received" ++ suffix) row.entity.totalReceived locale coinCode
        ++ Util.Csv.valuesWithBaseCurrencyFloat ("entity_balance" ++ suffix) row.entity.balance locale coinCode
        ++ Util.Csv.valuesWithBaseCurrencyFloat (estimatedValueTitle ++ suffix) row.value locale coinCode
        ++ (if coinCode == "eth" then
                prepareCsvTokens locale row

            else
                []
           )


prepareCsvTokens : Locale.Model -> Api.Data.NeighborEntity -> List ( ( String, List String ), String )
prepareCsvTokens locale row =
    Locale.tokenCurrencies locale
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
                    ++ Util.Csv.values ("value_" ++ token)
                        (row.tokenValues
                            |> Maybe.andThen (Dict.get token)
                            |> Maybe.withDefault zero
                        )
            )
        |> List.concat
