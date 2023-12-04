module View.Graph.Table.AddressTxsUtxoTable exposing (..)

import Api.Data
import Config.View as View
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Graph.Table exposing (Table, titleHeight, titleTimestamp, titleTx, titleValue)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
import Util.View exposing (longIdentifier)
import View.Graph.Table as T exposing (customizations)


config : View.Config -> String -> Table.Config Api.Data.AddressTxUtxo Msg
config vc coinCode =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                titleTx
                .txHash
                (\data ->
                    longIdentifier vc data.txHash
                        |> List.singleton
                        |> a
                            [ Css.View.link vc |> css
                            , Route.txRoute
                                { currency = coinCode
                                , txHash = data.txHash
                                , table = Nothing
                                , tokenTxId = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                                |> href
                            ]
                        |> List.singleton
                )
            , T.valueColumn vc (\_ -> coinCode) titleValue .value
            , T.intColumnWithoutValueDetailFormatting vc titleHeight .height
            , T.timestampColumn vc titleTimestamp .timestamp
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.AddressTxUtxo -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    [ ( ( "tx_hash", [] ), Util.Csv.string row.txHash )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "value" row.value locModel currency
        ++ [ ( ( "height", [] ), Util.Csv.int row.height )
           , ( ( "timestamp", [] ), Util.Csv.timestamp locModel row.timestamp )
           ]
