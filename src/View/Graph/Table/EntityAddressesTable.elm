module View.Graph.Table.EntityAddressesTable exposing (..)

import Api.Data
import Config.View as View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Msg.Graph exposing (Msg(..))
import Table
import View.Graph.Table as T exposing (customizations, valueColumn)


config : View.Config -> String -> Table.Config Api.Data.Address Msg
config vc coinCode =
    Table.customConfig
        { toId = .address
        , toMsg = TableNewState
        , columns =
            [ T.stringColumn vc "address" .address
            , T.valueColumn vc coinCode "balance" .balance
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
