module View.Pathfinder.Table.TransactionTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Model.Currency exposing (assetFromBase)
import Model.Graph.Table exposing (Table)
import Model.Locale
import Msg.Pathfinder exposing (Msg(..))
import Table
import View.Graph.Table as T exposing (customizations)
import View.Pathfinder.Table.Columns as PT


type alias GenericTx =
    { txHash : String, timestamp : Int, value : Api.Data.Values, asset : String }


toGerneric : Api.Data.AddressTx -> GenericTx
toGerneric x =
    case x of
        Api.Data.AddressTxAddressTxUtxo y ->
            GenericTx y.txHash y.timestamp y.value y.currency

        Api.Data.AddressTxTxAccount y ->
            GenericTx y.txHash y.timestamp y.value y.currency


getId : GenericTx -> String
getId =
    .txHash


config : View.Config -> Table.Config Api.Data.AddressTx Msg
config vc =
    Table.customConfig
        { toId = toGerneric >> getId
        , toMsg = \x -> NoOp
        , columns =
            [ PT.timestampDateMultiRowColumn vc
                "Timestamp"
                (toGerneric >> .timestamp)
            , PT.txColumn vc
                "Hash"
                (toGerneric >> .txHash)
            , T.valueColumn vc
                (toGerneric >> .asset >> assetFromBase)
                "Debit/Credit"
                (toGerneric >> .value)
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.AddressTx -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    []
