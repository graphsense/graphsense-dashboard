module View.Graph.Table.AddressTxsTable exposing (..)

import Api.Data
import Config.View as View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Msg.Graph exposing (Msg(..))
import Table
import View.Graph.Table as T exposing (customizations, valueColumn)


config : View.Config -> String -> Table.Config Api.Data.AddressTxUtxo Msg
config vc coinCode =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.stringColumn vc "transaction" .txHash
            , T.valueColumn vc coinCode "value" .value
            ]
        , customizations = customizations vc
        }



{-
     name: t('Height'),
     data: 'height',
     className: 'text-right'
   },
   {
     name: t('Timestamp'),
     data: 'timestamp',
     render: this.formatValue(this.formatTimestamp)
-}
