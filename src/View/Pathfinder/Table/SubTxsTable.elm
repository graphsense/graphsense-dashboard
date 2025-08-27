module View.Pathfinder.Table.SubTxsTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Css.Table exposing (Styles)
import Dict
import Init.Pathfinder.Id as Id
import Model.Currency as Currency
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (TxDetailsMsg(..))
import RecordSetter as Rs
import Table
import Theme.Colors as Colors
import Theme.Html.SidePanelComponents as SidePanelComponents
import View.Pathfinder.PagedTable exposing (alignColumnHeader, customizations)
import View.Pathfinder.Table.Columns as PT


config : Styles -> View.Config -> (Id -> Bool) -> Table.Config Api.Data.TxAccount TxDetailsMsg
config styles vc isCheckedFn =
    let
        toId r =
            Id.init r.network r.identifier

        rightAlignedColumns =
            Dict.empty

        styles_ =
            styles
                |> Rs.s_headRow
                    (styles.headRow
                        >> flip (++)
                            [ Css.property "background-color" Colors.white
                            ]
                    )
                |> Rs.s_headCell
                    (styles.headCell
                        >> flip (++)
                            (SidePanelComponents.sidePanelListHeadCell_details.styles
                                ++ SidePanelComponents.sidePanelListHeadCellPlaceholder_details.styles
                                ++ [ Css.display Css.tableCell ]
                            )
                    )
                |> Rs.s_table
                    (styles.table >> flip (++) fullWidth)
    in
    Table.customConfig
        { toId = toId >> Id.toString
        , toMsg = \_ -> NoOpSubTxsTable
        , columns =
            [ PT.checkboxColumn vc
                { isChecked = toId >> isCheckedFn
                , onClick = UserClickedTxInSubTxsTable
                , readonly = \_ -> False
                }
            , PT.addressColumn vc "from Address" .fromAddress
            , PT.addressColumn vc "to Address" .toAddress
            , PT.valueColumnWithOptions { sortable = False, hideCode = False, colorFlowDirection = False, isOutgoingFn = \_ -> False } vc (\d -> Currency.asset d.network d.currency) "Value" .value
            ]
        , customizations = customizations vc |> alignColumnHeader styles_ vc rightAlignedColumns
        }
