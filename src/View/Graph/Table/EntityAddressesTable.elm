module View.Graph.Table.EntityAddressesTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Address as A
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.EntityAddressesTable exposing (..)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (copyableLongIdentifier, none)
import View.Graph.Table as T exposing (customizations)
import Model.Currency exposing (assetFromBase)


config : View.Config -> String -> Maybe EntityId -> (EntityId -> A.Address -> Bool) -> Table.Config Api.Data.Address Msg
config vc coinCode entityId entityHasAddress =
    Table.customConfig
        { toId = .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                titleAddress
                .address
                (\data ->
                    [ entityId
                        |> Maybe.map
                            (\id ->
                                T.tickIf vc
                                    (entityHasAddress id)
                                    { currency = data.currency, address = data.address }
                            )
                        |> Maybe.withDefault none
                    , copyableLongIdentifier vc
                        (entityId
                            |> Maybe.map
                                (\id ->
                                    [ UserClickedAddressInEntityAddressesTable id data
                                        |> onClick
                                    , css [ cursor pointer ]
                                    ]
                                )
                            |> Maybe.withDefault []
                        )
                        data.address
                    ]
                )
            , T.timestampColumn vc titleFirstUsage (.firstTx >> .timestamp)
            , T.timestampColumn vc titleLastUsage (.lastTx >> .timestamp)
            , T.valueColumn vc (\_ -> assetFromBase coinCode) titleFinalBalance .balance
            , T.valueColumn vc (\_ -> assetFromBase coinCode) titleTotalReceived .totalReceived
            ]
        , customizations = customizations vc
        }


n s =
    ( s, [] )


prepareCSV : Model.Locale.Model -> String -> Api.Data.Address -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    [ ( n "address", Util.Csv.string row.address )
    , ( n "first_usage", Util.Csv.timestamp locModel row.firstTx.timestamp )
    , ( n "last_usage", Util.Csv.timestamp locModel row.lastTx.timestamp )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "final_balance" row.totalReceived locModel currency
        ++ Util.Csv.valuesWithBaseCurrencyFloat "total_received" row.balance locModel currency
