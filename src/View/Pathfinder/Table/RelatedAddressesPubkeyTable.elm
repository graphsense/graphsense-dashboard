module View.Pathfinder.Table.RelatedAddressesPubkeyTable exposing (RelatedAddressesPubkeyTableConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Css.Table exposing (Styles)
import Dict
import Init.Pathfinder.Id as Id
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesPubkeyTable exposing (Model)
import Msg.Pathfinder.AddressDetails as AddressDetails
import RecordSetter as Rs
import Table
import Theme.Colors as Colors
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.View exposing (copyIconPathfinder, truncateLongIdentifier)
import View.Graph.Table exposing (htmlColumnWithSorter)
import View.Locale as Locale
import View.Pathfinder.PagedTable exposing (alignColumnHeader, customizations)
import View.Pathfinder.Table.Columns exposing (checkboxColumn, stringColumn)


type alias RelatedAddressesPubkeyTableConfig =
    { isChecked : Id -> Bool
    }


config : Styles -> View.Config -> RelatedAddressesPubkeyTableConfig -> Model -> Table.Config Api.Data.RelatedAddress AddressDetails.Msg
config styles vc ratc _ =
    let
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

        toId { currency, address } =
            Id.init currency address
    in
    Table.customConfig
        { toId = .address
        , toMsg = AddressDetails.RelatedAddressesPubkeyTableMsg
        , columns =
            [ checkboxColumn vc
                { isChecked = toId >> ratc.isChecked
                , onClick = toId >> AddressDetails.UserClickedAddressCheckboxInTable
                , readonly = \_ -> False
                }
            , htmlColumnWithSorter Table.unsortable
                styles
                vc
                (Locale.string vc.locale "Address")
                (\{ address } -> address)
                (\{ address } ->
                    [ SidePanelComponents.sidePanelListIdentifierCell
                        { root =
                            { identifier = truncateLongIdentifier address
                            , copyIconInstance = copyIconPathfinder vc address
                            }
                        }
                    ]
                )
            , stringColumn vc
                { label = Locale.string vc.locale "currency"
                , accessor = \{ currency } -> currency |> String.toUpper
                , onClick = Nothing
                }
            ]
        , customizations = customizations vc |> alignColumnHeader styles_ vc rightAlignedColumns
        }
