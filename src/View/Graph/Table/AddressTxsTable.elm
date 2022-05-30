module View.Graph.Table.AddressTxsTable exposing (..)

import Api.Data
import Config.View as View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


init : Table Api.Data.AddressTxUtxo
init =
    Init.Graph.Table.init "Transaction"


config : View.Config -> String -> Table.Config Api.Data.AddressTxUtxo Msg
config vc coinCode =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.stringColumn vc "Transaction" .txHash
            , T.valueColumn vc coinCode "Value" .value
            , T.intColumn vc "Height" .height
            , T.timestampColumn vc "Timestamp" .timestamp
            ]
        , customizations = customizations vc
        }
