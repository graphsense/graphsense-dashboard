module View.Graph.Table.AddresslinkTxsUtxoTable exposing (..)

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


init : Table Api.Data.LinkUtxo
init =
    Init.Graph.Table.init filter "Transaction"


filter : String -> Api.Data.LinkUtxo -> Bool
filter f a =
    String.contains f a.txHash
        || String.contains f (String.fromInt a.height)


config : View.Config -> String -> Table.Config Api.Data.LinkUtxo Msg
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
            , T.valueColumn vc coinCode "Input value" .inputValue
            , T.valueColumn vc coinCode "Output value" .outputValue
            , T.intColumn vc "Height" .height
            , T.timestampColumn vc "Timestamp" .timestamp
            ]
        , customizations = customizations vc
        }


prepareCSV : Api.Data.LinkUtxo -> List ( String, String )
prepareCSV row =
    Debug.todo "prepareCSV"
