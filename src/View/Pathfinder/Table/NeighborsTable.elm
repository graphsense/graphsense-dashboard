module View.Pathfinder.Table.NeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Model.Currency exposing (asset)
import Model.Locale
import Msg.Pathfinder exposing (Msg(..))
import Table
import View.Graph.Table exposing (customizations)
import View.Pathfinder.Table.Columns as PT


getAddress : Api.Data.NeighborAddress -> String
getAddress n =
    n.address.address


config : View.Config -> String -> Table.Config Api.Data.NeighborAddress Msg
config vc network =
    Table.customConfig
        { toId = getAddress
        , toMsg = \_ -> NoOp
        , columns =
            [ PT.addressColumn vc
                "Address"
                getAddress
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.AddressTx -> List ( ( String, List String ), String )
prepareCSV _ _ _ =
    []
