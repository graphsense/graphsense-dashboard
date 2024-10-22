module View.Pathfinder.Table.TransactionTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Init.Pathfinder.Id as Id
import Model.Currency exposing (asset)
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter as Rs
import Set
import Table
import Theme.Html.SidePanelComponents as SidePanelComponents
import View.Pathfinder.PagedTable exposing (alignColumnsRight, customizations)
import View.Pathfinder.Table.Columns as PT


type alias GenericTx =
    { network : String
    , txHash : String
    , id : String
    , timestamp : Int
    , value : Api.Data.Values
    , asset : String
    , isOutgoing : Bool
    }


toGerneric : Id -> Api.Data.AddressTx -> GenericTx
toGerneric addressId x =
    case x of
        Api.Data.AddressTxAddressTxUtxo y ->
            GenericTx y.currency y.txHash y.txHash y.timestamp y.value y.currency (y.value.value <= 0)

        Api.Data.AddressTxTxAccount y ->
            GenericTx y.network y.txHash y.identifier y.timestamp y.value y.currency (y.fromAddress == Id.id addressId)


getId : GenericTx -> Id
getId { network, id } =
    Id.init network id


config : Styles -> View.Config -> Id -> (Id -> Bool) -> Table.Config Api.Data.AddressTx Msg
config styles vc addressId isCheckedFn =
    let
        network =
            Id.network addressId

        rightAlignedColumns =
            Set.singleton "Value"

        styles_ =
            styles
                |> Rs.s_headCell
                    (styles.headCell
                        >> flip (++)
                            (SidePanelComponents.sidePanelListHeadCell_details.styles
                                ++ SidePanelComponents.sidePanelListHeadCellPlaceholder_details.styles
                                ++ [ Css.display Css.tableCell ]
                            )
                    )
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
                |> alignColumnsRight styles_ vc rightAlignedColumns
        }
