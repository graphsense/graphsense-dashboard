module View.Graph.Table.AddresslinkTxsUtxoTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Css.Table exposing (styles)
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Currency exposing (assetFromBase)
import Model.Graph.Table.AddresslinkTxsUtxoTable exposing (..)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
import Util.View
import View.Graph.Table as T exposing (customizations)


config : View.Config -> String -> Table.Config Api.Data.LinkUtxo Msg
config vc coinCode =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
                titleTx
                .txHash
                (\data ->
                    Util.View.truncate vc.theme.table.urlMaxLength data.txHash
                        |> text
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
            , T.valueColumn styles vc (\_ -> assetFromBase coinCode) titleInputValue .inputValue
            , T.valueColumn styles vc (\_ -> assetFromBase coinCode) titleOutputValue .outputValue
            , T.intColumnWithoutValueDetailFormatting styles vc titleHeight .height
            , T.timestampColumn styles vc titleTimestamp .timestamp
            ]
        , customizations = customizations styles vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.LinkUtxo -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    ( ( "tx_hash", [] ), Util.Csv.string row.txHash )
        :: Util.Csv.valuesWithBaseCurrencyFloat "input_value" row.inputValue locModel currency
        ++ Util.Csv.valuesWithBaseCurrencyFloat "output_value" row.outputValue locModel currency
        ++ [ ( ( "height", [] ), Util.Csv.int row.height )
           , ( ( "timestamp", [] ), Util.Csv.timestamp locModel row.timestamp )
           ]
