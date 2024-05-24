module View.Pathfinder.Table.TransactionTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Model.Currency exposing (asset)
import Model.Locale
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Table
import View.Pathfinder.Table exposing (customizations)
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


config : View.Config -> String -> (Id -> Bool) -> Table.Config Api.Data.AddressTx Msg
config vc network isCheckedFn =
    Table.customConfig
        { toId = toGerneric >> getId
        , toMsg = \_ -> NoOp
        , columns =
            [ PT.checkboxColumn vc
                ""
                (toGerneric >> (\x -> isCheckedFn ( network, x.txHash )))
                UserClickedTxCheckboxInTable
            , PT.timestampDateMultiRowColumn vc
                "Timestamp"
                (toGerneric >> .timestamp)
            , PT.txColumn vc
                "Hash"
                (toGerneric >> .txHash)
            , PT.debitCreditColumn vc
                (toGerneric >> .asset >> asset network)
                "Debit/Credit"
                (toGerneric >> .value)
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.AddressTx -> List ( ( String, List String ), String )
prepareCSV _ _ _ =
    []
