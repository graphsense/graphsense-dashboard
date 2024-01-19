module View.Graph.Table.TxsUtxoTable exposing (..)

import Api.Data
import Config.View as View
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Currency exposing (assetFromBase)
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.TxsUtxoTable exposing (..)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
import Util.View
import View.Graph.Table as T exposing (customizations)


config : View.Config -> String -> Table.Config Api.Data.TxUtxo Msg
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
            , T.intColumn vc titleNoInputs .noInputs
            , T.intColumn vc titleNoOutputs .noOutputs
            , T.valueColumn vc (\_ -> assetFromBase coinCode) titleTotalInput .totalInput
            , T.valueColumn vc (\_ -> assetFromBase coinCode) titleTotalOutput .totalOutput
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.TxUtxo -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    [ ( ( "tx_hash", [] ), Util.Csv.string row.txHash )
    , ( ( "no_inputs", [] ), Util.Csv.int row.noInputs )
    , ( ( "no_outputs", [] ), Util.Csv.int row.noOutputs )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "total_input" row.totalInput locModel currency
        ++ Util.Csv.valuesWithBaseCurrencyFloat "total_output" row.totalOutput locModel currency
