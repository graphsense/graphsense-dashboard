module View.Pathfinder.Table.TransactionTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Css.Table exposing (Styles)
import Init.Pathfinder.Id as Id
import Model.Currency exposing (asset)
import Model.Locale
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Set
import Table
import View.Pathfinder.PagedTable exposing (alignColumnsRight, customizations)
import View.Pathfinder.Table.Columns as PT
import Model.Pathfinder.Id exposing (network)
import View.Graph.Search exposing (direction)
import Api.Request.Addresses exposing (Direction)
import Model.Pathfinder.Tx as Tx
import View.Graph.Address exposing (address)


type alias GenericTx =
    { network : String
    , txHash : String
    , id : String
    , timestamp : Int
    , value : Api.Data.Values
    , asset : String
    , isOutgoing : Bool
    }


toGerneric : Id ->  Api.Data.AddressTx -> GenericTx
toGerneric addressId x =
    case x of
        Api.Data.AddressTxAddressTxUtxo y ->
            GenericTx y.currency y.txHash y.txHash y.timestamp y.value y.currency (y.value.value <= 0)

        Api.Data.AddressTxTxAccount y ->
            GenericTx y.network y.txHash y.identifier y.timestamp y.value y.currency (y.fromAddress == Id.id addressId)


getId : GenericTx -> Id
getId { network, id } =
    Id.init network id


config : Styles -> View.Config ->  Id -> (Id -> Bool) -> Table.Config Api.Data.AddressTx Msg
config _ vc addressId isCheckedFn =
    let

        network = Id.network addressId
        rightAlignedColumns =
            [ "Value" ]
    in
    Table.customConfig
        { toId = toGerneric addressId >> getId >> Id.toString
        , toMsg = \_ -> NoOp
        , columns =
            [ PT.checkboxColumn vc
                { isChecked = toGerneric addressId >> getId >> isCheckedFn
                , onClick = UserClickedTxCheckboxInTable
                }
            , PT.timestampDateMultiRowColumn vc
                "Timestamp"
                (toGerneric addressId >> .timestamp)
            , PT.txColumn vc
                { label = "Hash"
                , accessor = toGerneric addressId >> .txHash
                , onClick = Just (toGerneric addressId >> getId >> UserClickedTx)
                , tagsPlaceholder = False
                }
            , PT.debitCreditColumn 
                (toGerneric addressId >> .isOutgoing)
                vc
                (toGerneric addressId >> .asset >> asset network)
                "Value"
                (toGerneric addressId >> .value)
            ]
        , customizations =
            customizations vc
                |> alignColumnsRight vc (Set.fromList rightAlignedColumns)
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.AddressTx -> List ( ( String, List String ), String )
prepareCSV _ _ _ =
    []
