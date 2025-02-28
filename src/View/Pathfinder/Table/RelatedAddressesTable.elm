module View.Pathfinder.Table.RelatedAddressesTable exposing (RelatedAddressesTableConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Css.View
import Dict
import Html.Styled as Html
import Init.Pathfinder.Id as Id
import Model.Currency exposing (AssetIdentifier)
import Model.Pathfinder exposing (HavingTags(..), getSortedConceptsByWeight)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable exposing (Model, totalReceivedColumn)
import Msg.Pathfinder.AddressDetails as AddressDetails
import RecordSetter as Rs
import Table
import Theme.Colors as Colors
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Tag as Tag
import Util.View exposing (copyIconPathfinder, loadingSpinner, truncateLongIdentifier)
import View.Graph.Table exposing (htmlColumnWithSorter)
import View.Locale as Locale
import View.Pathfinder.PagedTable exposing (alignColumnHeader, customizations)
import View.Pathfinder.Table.Columns exposing (checkboxColumn, twoValuesCell)


type alias RelatedAddressesTableConfig =
    { coinCode : AssetIdentifier
    , isChecked : Id -> Bool
    , hasTags : Id -> HavingTags
    }


config : Styles -> View.Config -> RelatedAddressesTableConfig -> Model -> Table.Config Api.Data.Address AddressDetails.Msg
config styles vc ratc _ =
    let
        rightAlignedColumns =
            Dict.fromList [ ( totalReceivedColumn, View.Pathfinder.PagedTable.RightAligned ) ]

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

        toId { currency, address } =
            Id.init currency address
    in
    Table.customConfig
        { toId = .address
        , toMsg = AddressDetails.RelatedAddressesTableMsg
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
                        { sidePanelListIdentifierCell =
                            { identifier = truncateLongIdentifier address
                            , copyIconInstance = copyIconPathfinder vc address
                            }
                        }
                    ]
                )
            , htmlColumnWithSorter Table.unsortable
                styles
                vc
                (Locale.string vc.locale "Category")
                (\{ address } -> address)
                (\data ->
                    case toId data |> ratc.hasTags of
                        NoTags ->
                            []

                        HasTags _ ->
                            []

                        HasTagSummary ts ->
                            getSortedConceptsByWeight ts
                                |> List.head
                                |> Maybe.map
                                    (Tag.conceptItem vc (toId data)
                                        >> Html.map AddressDetails.TooltipMsg
                                        >> List.singleton
                                    )
                                |> Maybe.withDefault []

                        LoadingTags ->
                            [ loadingSpinner vc Css.View.loadingSpinner
                            ]

                        HasExchangeTagOnly ->
                            []
                )
            , twoValuesCell vc
                (Locale.string vc.locale "Total received")
                { coinCode = ratc.coinCode
                , getValue1 = .totalReceived
                , getValue2 = .balance
                , labelValue2 = Locale.string vc.locale "Balance"
                }
            ]
        , customizations = customizations vc |> alignColumnHeader styles_ vc rightAlignedColumns
        }
