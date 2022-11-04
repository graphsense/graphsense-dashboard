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
import Util.View
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


init : Table Api.Data.TxUtxo
init =
    Init.Graph.Table.init filter "Transaction"


filter : String -> Api.Data.TxUtxo -> Bool
filter f a =
    String.contains f a.txHash


config : View.Config -> String -> Table.Config Api.Data.TxUtxo Msg
config vc coinCode =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                "Transaction"
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
            , T.intColumn vc "No. inputs" .noInputs
            , T.intColumn vc "No. outputs" .noOutputs
            , T.valueColumn vc coinCode "Total input" .totalInput
            , T.valueColumn vc coinCode "Total output" .totalOutput
            ]
        , customizations = customizations vc
        }


prepareCSV : Api.Data.TxUtxo -> List ( String, String )
prepareCSV row =
    Debug.todo "prepareCSV"
