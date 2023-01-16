module View.Graph.Table.TxsUtxoTable exposing (..)

import Api.Data
import Config.View as View
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
import Util.View
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


init : Table Api.Data.TxUtxo
init =
    Init.Graph.Table.init filter "Transaction"


filter : String -> Api.Data.TxUtxo -> Bool
filter f a =
    String.contains f a.txHash


titleTx : String
titleTx =
    "Transaction"


titleNoInputs : String
titleNoInputs =
    "No. inputs"


titleNoOutputs : String
titleNoOutputs =
    "No. outputs"


titleTotalInput : String
titleTotalInput =
    "Total input"


titleTotalOutput : String
titleTotalOutput =
    "Total output"


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
                                }
                                |> Route.graphRoute
                                |> toUrl
                                |> href
                            ]
                        |> List.singleton
                )
            , T.intColumn vc titleNoInputs .noInputs
            , T.intColumn vc titleNoOutputs .noOutputs
            , T.valueColumn vc (\_ -> coinCode) titleTotalInput .totalInput
            , T.valueColumn vc (\_ -> coinCode) titleTotalOutput .totalOutput
            ]
        , customizations = customizations vc
        }


prepareCSV : Api.Data.TxUtxo -> List ( ( String, List String ), String )
prepareCSV row =
    [ ( ( "tx_hash", [] ), Util.Csv.string row.txHash )
    , ( ( "no_inputs", [] ), Util.Csv.int row.noInputs )
    , ( ( "no_outputs", [] ), Util.Csv.int row.noOutputs )
    ]
        ++ Util.Csv.values "total_input" row.totalInput
        ++ Util.Csv.values "total_output" row.totalOutput
