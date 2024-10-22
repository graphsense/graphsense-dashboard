module View.Pathfinder.Table.IoTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Init.Pathfinder.Id as Id
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (IoDirection, Msg(..), TxDetailsMsg(..))
import RecordSetter as Rs
import Set
import Table
import Theme.Colors as Colors
import Theme.Html.SidePanelComponents as SidePanelComponents
import View.Graph.Table exposing (customizations)
import View.Pathfinder.PagedTable exposing (alignColumnsRight)
import View.Pathfinder.Table.Columns as PT


config : Styles -> View.Config -> IoDirection -> String -> (Id -> Bool) -> Maybe (Id -> HavingTags) -> Table.Config Api.Data.TxValue Msg
config styles vc ioDirection network isCheckedFn lblFn =
    let
        toId =
            .address
                >> List.head
                >> Maybe.map (Id.init network)

        rightAlignedColumns =
            Set.singleton "Value"

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
    in
    Table.customConfig
        { toId = .address >> String.concat
        , toMsg = TableMsg ioDirection >> TxDetailsMsg
        , columns =
            [ PT.checkboxColumn vc
                { isChecked =
                    toId
                        >> Maybe.map isCheckedFn
                        >> Maybe.withDefault False
                , onClick =
                    toId >> Maybe.map UserClickedAddressCheckboxInTable >> Maybe.withDefault NoOp
                }
            , PT.addressColumn vc
                { label = "Address"
                , accessor = .address >> String.join ","
                , onClick = Just (toId >> Maybe.map UserClickedAddress >> Maybe.withDefault NoOp)
                , tagsPlaceholder = True
                }
                (lblFn |> Maybe.map (\fn -> \data -> toId data |> Maybe.map fn |> Maybe.withDefault NoTags))
            , PT.sortableDebitCreditColumn
                (.value >> .value >> (>=) 0)
                vc
                (\_ -> assetFromBase network)
                "Value"
                .value
            ]
        , customizations = customizations styles_ vc |> alignColumnsRight styles_ vc rightAlignedColumns
        }
