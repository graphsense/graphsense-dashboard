module View.Graph.Table.TxsUtxoTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Css.Table exposing (styles)
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Currency exposing (assetFromBase)
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
            , T.intColumn styles vc titleNoInputs .noInputs
            , T.intColumn styles vc titleNoOutputs .noOutputs
            , T.valueColumn styles vc (\_ -> assetFromBase coinCode) titleTotalInput .totalInput
            , T.valueColumn styles vc (\_ -> assetFromBase coinCode) titleTotalOutput .totalOutput
            ]
        , customizations = customizations styles vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.TxUtxo -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    [ ( ( "tx_hash", [] ), Util.Csv.string row.txHash )
    , ( ( "no_inputs", [] ), Util.Csv.int row.noInputs )
    , ( ( "no_outputs", [] ), Util.Csv.int row.noOutputs )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "total_input" row.totalInput locModel currency
        ++ Util.Csv.valuesWithBaseCurrencyFloat "total_output" row.totalOutput locModel currency
