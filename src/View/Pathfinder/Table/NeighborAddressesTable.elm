module View.Pathfinder.Table.NeighborAddressesTable exposing (NeighborAddressesTableConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Css.Table exposing (Styles)
import Css.View
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
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
import View.Pathfinder.InfiniteTable as InfiniteTable
import View.Pathfinder.PagedTable exposing (customizations)
import View.Pathfinder.Table.Columns exposing (addHeaderAttributes, checkboxColumn, valueColumnWithOptions)


type alias NeighborAddressesTableConfig =
    { anchorId : Id
    , coinCode : AssetIdentifier
    , isChecked : ( Id, Id ) -> Bool
    , hasTags : Id -> HavingTags
    , direction : Direction
    }


config : Styles -> View.Config -> NeighborAddressesTableConfig -> InfiniteTable.TableConfig Api.Data.NeighborAddress AddressDetails.Msg
config styles vc conf =
    let
        cellLabel =
            case conf.direction of
                Outgoing ->
                    "Total received"

                Incoming ->
                    "Total sent"

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
    { toId = .address >> .address
    , columns =
        [ checkboxColumn vc
            { isChecked = toAggId >> conf.isChecked
            , onClick = AddressDetails.UserClickedAggEdgeCheckboxInTable conf.direction conf.anchorId
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
                                    >> Html.map (AddressDetails.TagTooltipMsg >> AddressDetails.TooltipMsg)
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
        , valueColumnWithOptions
            { sortable = False
            , hideCode = True
            , colorFlowDirection = False
            , isOutgoingFn = \_ -> False
            }
            vc
            (.address >> .currency >> Currency.assetFromBase)
            (Locale.string vc.locale cellLabel)
            .value
        ]
    , customizations =
        customizations vc
            |> addHeaderAttributes styles_ vc cellLabel [ css [ Css.textAlign Css.right ] ]
    , tag = AddressDetails.NeighborsTableSubTableMsg conf.direction
    , loadingPlaceholderAbove = InfiniteTable.loadingPlaceholderAbove vc
    , loadingPlaceholderBelow = InfiniteTable.loadingPlaceholderBelow vc
    }
