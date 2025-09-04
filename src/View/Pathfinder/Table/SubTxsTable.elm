module View.Pathfinder.Table.SubTxsTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Css.Table exposing (Styles)
import Html.Styled.Attributes exposing (css)
import Init.Pathfinder.Id as Id
import Model.Currency as Currency
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (TxDetailsMsg(..))
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.SidePanelComponents as SidePanelComponents
import View.Pathfinder.InfiniteTable as InfiniteTable
import View.Pathfinder.PagedTable exposing (customizations)
import View.Pathfinder.Table.Columns as PT exposing (addHeaderAttributes, applyHeaderCustomizations, initCustomHeaders)


titleValue : String
titleValue =
    "Value"


config : Styles -> View.Config -> { selectedSubTx : Id, isCheckedFn : Id -> Bool } -> InfiniteTable.TableConfig Api.Data.TxAccount TxDetailsMsg
config styles vc { selectedSubTx, isCheckedFn } =
    let
        toId r =
            Id.init r.network r.identifier

        rowBaseAttrs =
            [ Css.height (Css.px 52)
            , Css.property "border" ("1px solid " ++ Colors.grey50)
            ]

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
    { toId = toId >> Id.toString
    , columns =
        [ PT.checkboxColumn vc
            ""
            { isChecked = toId >> isCheckedFn
            , onClick = UserClickedTxInSubTxsTable
            , readonly = \_ -> False
            }
        , PT.addressColumn vc { name = "from Address", withCopy = False } .fromAddress
        , PT.addressColumn vc { name = "to Address", withCopy = False } .toAddress
        , PT.valueColumnWithOptions { sortable = False, hideCode = False, colorFlowDirection = False, isOutgoingFn = \_ -> False } vc (\d -> Currency.asset d.network d.currency) titleValue .value
        ]
    , customizations =
        initCustomHeaders
            |> addHeaderAttributes titleValue [ css [ Css.textAlign Css.right ] ]
            |> flip (applyHeaderCustomizations styles_ vc) (customizations vc)
            |> Rs.s_rowAttrs
                (\d ->
                    (if (d |> toId) == selectedSubTx then
                        rowBaseAttrs
                            ++ [ Css.property "background-color" Colors.blue50
                               ]

                     else
                        rowBaseAttrs
                    )
                        |> css
                        |> List.singleton
                )
    , tag = TableMsgSubTxTable
    , loadingPlaceholderAbove = InfiniteTable.loadingPlaceholderAbove vc
    , loadingPlaceholderBelow = InfiniteTable.loadingPlaceholderBelow vc
    }
