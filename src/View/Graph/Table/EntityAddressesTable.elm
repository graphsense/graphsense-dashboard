module View.Graph.Table.EntityAddressesTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Css.Table exposing (styles)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Address as A
import Model.Currency exposing (assetFromBase)
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Table.EntityAddressesTable exposing (..)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (copyableLongIdentifier, none)
import View.Graph.Table as T exposing (customizations)


config : View.Config -> String -> Maybe EntityId -> (EntityId -> A.Address -> Bool) -> Table.Config Api.Data.Address Msg
config vc coinCode entityId entityHasAddress =
    Table.customConfig
        { toId = .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
                titleAddress
                .address
                (\data ->
                    [ entityId
                        |> Maybe.map
                            (\id ->
                                T.tickIf styles
                                    vc
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
            , T.timestampColumn styles vc titleFirstUsage (.firstTx >> .timestamp)
            , T.timestampColumn styles vc titleLastUsage (.lastTx >> .timestamp)
            , T.valueColumn styles vc (\_ -> assetFromBase coinCode) titleFinalBalance .balance
            , T.valueColumn styles vc (\_ -> assetFromBase coinCode) titleTotalReceived .totalReceived
            ]
        , customizations = customizations styles vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.Address -> List ( String, String )
prepareCSV locModel network row =
    [ ( "address", Util.Csv.string row.address )
    , ( "first_usage", Util.Csv.timestamp locModel row.firstTx.timestamp )
    , ( "last_usage", Util.Csv.timestamp locModel row.lastTx.timestamp )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "final_balance" row.totalReceived locModel (assetFromBase network)
        ++ Util.Csv.valuesWithBaseCurrencyFloat "total_received" row.balance locModel (assetFromBase network)
