module View.Pathfinder.Table.IoTable exposing (..)

import Api.Data
import Config.View as View
import Model.Currency exposing (assetFromBase)
import Msg.Pathfinder exposing (Msg(..))
import Table
import View.Pathfinder.Table exposing (customizations)
import View.Pathfinder.Table.Columns as PT


config : View.Config -> String -> Table.Config Api.Data.TxValue Msg
config vc network =
    Table.customConfig
        { toId = \_ -> ""
        , toMsg = \_ -> NoOp
        , columns =
            [ PT.addressColumn vc
                "Address"
                (.address >> String.join ",")
            , PT.debitCreditColumn vc
                (\_ -> assetFromBase network)
                "Value"
                .value
            ]
        , customizations = customizations vc
        }
