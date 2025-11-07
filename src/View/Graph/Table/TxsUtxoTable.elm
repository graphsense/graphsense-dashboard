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
import Time
import Util.Csv
import Util.Data as Data
import Util.View
import View.Graph.Table as T exposing (customizations)
import View.Locale as Locale


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


prepareCSV : Model.Locale.Model -> String -> Api.Data.TxUtxo -> List ( String, String )
prepareCSV locModel network row =
    [ ( "Tx_hash", Util.Csv.string row.txHash )
    , ( "Currency", Util.Csv.string <| String.toUpper row.currency )
    , ( "No_inputs", Util.Csv.int row.noInputs )
    , ( "No_outputs", Util.Csv.int row.noOutputs )
    , ( "Timestamp_utc", Locale.timestampNormal { locModel | zone = Time.utc } <| Data.timestampToPosix row.timestamp )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "Total_input" row.totalInput locModel (assetFromBase network)
        ++ Util.Csv.valuesWithBaseCurrencyFloat "Total_output" row.totalOutput locModel (assetFromBase network)
