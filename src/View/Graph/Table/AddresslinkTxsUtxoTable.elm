module View.Graph.Table.AddresslinkTxsUtxoTable exposing (..)

import Api.Data
import Config.View as View
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
import Util.View
import View.Graph.Table as T exposing (customizations)


init : Table Api.Data.LinkUtxo
init =
    Init.Graph.Table.initSorted True filter titleTimestamp


filter : String -> Api.Data.LinkUtxo -> Bool
filter f a =
    String.contains f a.txHash
        || String.contains f (String.fromInt a.height)


titleTx : String
titleTx =
    "Transaction"


titleInputValue : String
titleInputValue =
    "Input value"


titleOutputValue : String
titleOutputValue =
    "Output value"


titleHeight : String
titleHeight =
    "Height"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


config : View.Config -> String -> Table.Config Api.Data.LinkUtxo Msg
config vc coinCode =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
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
            , T.valueColumn vc (\_ -> coinCode) titleInputValue .inputValue
            , T.valueColumn vc (\_ -> coinCode) titleOutputValue .outputValue
            , T.intColumnWithoutValueDetailFormatting vc titleHeight .height
            , T.timestampColumn vc titleTimestamp .timestamp
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.LinkUtxo -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    [ ( ( "tx_hash", [] ), Util.Csv.string row.txHash )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "input_value" row.inputValue locModel currency
        ++ Util.Csv.valuesWithBaseCurrencyFloat "output_value" row.outputValue locModel currency
        ++ [ ( ( "height", [] ), Util.Csv.int row.height )
           , ( ( "timestamp", [] ), Util.Csv.timestamp locModel row.timestamp )
           ]
