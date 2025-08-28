module View.Pathfinder.Table.RelatedAddressesTable exposing (RelatedAddressesTableConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
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
import View.Pathfinder.InfiniteTable as InfiniteTable
import View.Pathfinder.PagedTable exposing (alignColumnHeader, customizations)
import View.Pathfinder.Table.Columns exposing (checkboxColumn, twoValuesColumn)


type alias RelatedAddressesTableConfig =
    { coinCode : AssetIdentifier
    , isChecked : Id -> Bool
    , hasTags : Id -> HavingTags
    }


config : Styles -> View.Config -> RelatedAddressesTableConfig -> Model -> InfiniteTable.TableConfig Api.Data.Address AddressDetails.Msg
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
                |> Rs.s_table
                    (styles.table >> flip (++) fullWidth)

        toId { currency, address } =
            Id.init currency address
    in
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
                    { root =
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
                let
                    withTagSummary ts =
                        getSortedConceptsByWeight ts
                            |> List.head
                            |> Maybe.map
                                (Tag.conceptItem vc (toId data)
                                    >> Html.map (AddressDetails.TagTooltipMsg >> AddressDetails.TooltipMsg)
                                    >> List.singleton
                                )
                            |> Maybe.withDefault []
                in
                case toId data |> ratc.hasTags of
                    NoTags ->
                        []

                    NoTagsWithoutCluster ->
                        []

                    HasTags _ ->
                        []

                    HasTagSummaryWithCluster _ ->
                        []

                    HasTagSummaryOnlyWithCluster _ ->
                        []

                    HasTagSummaryWithoutCluster ts ->
                        withTagSummary ts

                    HasTagSummaries { withoutCluster } ->
                        withTagSummary withoutCluster

                    LoadingTags ->
                        [ loadingSpinner vc Css.View.loadingSpinner
                        ]

                    HasExchangeTagOnly ->
                        []
            )
        , twoValuesColumn vc
            (Locale.string vc.locale "Total received")
            { coinCode = ratc.coinCode
            , getValue1 = .totalReceived
            , getValue2 = .balance
            , labelValue2 = Locale.string vc.locale "Balance"
            }
        ]
    , customizations = customizations vc |> alignColumnHeader styles_ vc rightAlignedColumns
    , tag = AddressDetails.RelatedAddressesTableSubTableMsg
    , rowHeight = 36
    , containerHeight = 300
    , loadingPlaceholderAbove = InfiniteTable.loadingPlaceholderAbove vc
    , loadingPlaceholderBelow = InfiniteTable.loadingPlaceholderBelow vc
    }
