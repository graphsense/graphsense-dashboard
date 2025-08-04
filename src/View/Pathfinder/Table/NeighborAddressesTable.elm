module View.Pathfinder.Table.NeighborAddressesTable exposing (NeighborAddressesTableConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Css.Table exposing (Styles)
import Css.View
import Dict
import Html.Styled as Html
import Init.Pathfinder.AggEdge as AggEdge
import Init.Pathfinder.Id as Id
import Model.Currency as Currency exposing (AssetIdentifier)
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (HavingTags(..), getSortedConceptsByWeight)
import Model.Pathfinder.Id exposing (Id)
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
import View.Pathfinder.Table.Columns exposing (checkboxColumn, valueColumn)


type alias NeighborAddressesTableConfig =
    { anchorId : Id
    , coinCode : AssetIdentifier
    , isChecked : ( Id, Id ) -> Bool
    , hasTags : Id -> HavingTags
    , direction : Direction
    }


config : Styles -> View.Config -> NeighborAddressesTableConfig -> Table.Config Api.Data.NeighborAddress AddressDetails.Msg
config styles vc conf =
    let
        cellLabel =
            case conf.direction of
                Outgoing ->
                    "Total received"

                Incoming ->
                    "Total sent"

        rightAlignedColumns =
            Dict.fromList [ ( cellLabel, View.Pathfinder.PagedTable.RightAligned ) ]

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

        toId nb =
            Id.init nb.address.currency nb.address.address

        toAggId nb =
            toId nb
                |> AggEdge.initId conf.anchorId
    in
    Table.customConfig
        { toId = .address >> .address
        , toMsg = \_ -> AddressDetails.NoOp
        , columns =
            [ checkboxColumn vc
                { isChecked = toAggId >> conf.isChecked
                , onClick = toId >> AddressDetails.UserClickedAggEdgeCheckboxInTable conf.anchorId
                , readonly = \_ -> False
                }
            , htmlColumnWithSorter Table.unsortable
                styles
                vc
                (Locale.string vc.locale "Address")
                (\{ address } -> address.address)
                (\{ address } ->
                    [ SidePanelComponents.sidePanelListIdentifierCell
                        { root =
                            { identifier = truncateLongIdentifier address.address
                            , copyIconInstance = copyIconPathfinder vc address.address
                            }
                        }
                    ]
                )
            , htmlColumnWithSorter Table.unsortable
                styles
                vc
                (Locale.string vc.locale "Category")
                (\{ address } -> address.address)
                (\data ->
                    let
                        withTagSummary ts =
                            getSortedConceptsByWeight ts
                                |> List.head
                                |> Maybe.map
                                    (Tag.conceptItem vc (toId data)
                                        >> Html.map AddressDetails.TooltipMsg
                                        >> List.singleton
                                    )
                                |> Maybe.withDefault []
                    in
                    case toId data |> conf.hasTags of
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
            , valueColumn vc
                (.address >> .currency >> Currency.assetFromBase)
                (Locale.string vc.locale cellLabel)
                .value
            ]
        , customizations = customizations vc |> alignColumnHeader styles_ vc rightAlignedColumns
        }
