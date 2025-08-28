module View.Pathfinder.Table.TransactionTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder as PCSS
import Css.Table exposing (Styles)
import Dict
import Html.Styled exposing (td, th)
import Html.Styled.Attributes exposing (css)
import Init.Pathfinder.Id as Id
import Model.Currency exposing (asset)
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter as Rs
import Table
import Theme.Colors
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Checkbox
import Util.View exposing (copyIconPathfinder, truncateLongIdentifierWithLengths)
import View.Pathfinder.PagedTable as PT exposing (addTHeadOverwrite, alignColumnHeader, customizations)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, wrapCell)
import View.Pathfinder.InfiniteTable as InfiniteTable


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


config : Styles -> View.Config -> Id -> (Id -> Bool) -> Bool -> InfiniteTable.TableConfig Api.Data.AddressTx Msg
config styles vc addressId isCheckedFn allChecked =
    let
        network =
            Id.network addressId

        rightAlignedColumns =
            Dict.fromList [ ( "Value", PT.RightAligned ) ]

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

        c =
            customizations vc
                |> alignColumnHeader styles_ vc rightAlignedColumns

        addAllCheckbox =
            Util.Checkbox.checkbox
                { state = Util.Checkbox.stateFromBool allChecked
                , size = Util.Checkbox.smallSize
                , msg = UserClickedAllTxCheckboxInTable
                }
                ([ Css.paddingLeft <| Css.px 5 ]
                    |> css
                    |> List.singleton
                )

        newTheadWithCheckbox =
            addTHeadOverwrite ""
                (\( _, _, a ) ->
                    Table.HtmlDetails
                        [ a
                        , [ PCSS.mGap |> Css.padding
                          , Css.width <| Css.px 50
                          , Css.property "background-color" Theme.Colors.white
                          ]
                            |> css
                        ]
                        [ th [] [ td [] [ addAllCheckbox ] ] ]
                )
                c.thead

        cc =
            c |> Rs.s_thead newTheadWithCheckbox
    in
    { toId = toGerneric addressId >> getId >> Id.toString
    , toMsg = \_ -> NoOp
    , columns =
        [ PT.checkboxColumn vc
            { isChecked = toGerneric addressId >> getId >> isCheckedFn
            , onClick = UserClickedTxCheckboxInTable
            , readonly = \_ -> False
            }
        , PT.timestampDateMultiRowColumn vc
            "Timestamp"
            (toGerneric addressId >> .timestamp)
        , txColumn vc
            { label = "Hash"
            , accessor = toGerneric addressId >> .txHash
            , onClick = Just (toGerneric addressId >> getId >> UserClickedTx)
            }
        , PT.debitCreditColumn
            (toGerneric addressId >> .isOutgoing)
            vc
            (toGerneric addressId >> .asset >> asset network)
            "Value"
            (toGerneric addressId >> .value)
        ]
    , customizations = cc
    , tag = TransactionsTableSubTableMsg
    , rowHeight = 38
    , containerHeight = 300
    , loadingPlaceholderAbove = InfiniteTable.loadingPlaceholderAbove vc
    , loadingPlaceholderBelow = InfiniteTable.loadingPlaceholderBelow vc
    }


txColumn : View.Config -> ColumnConfig Api.Data.AddressTx msg -> Table.Column Api.Data.AddressTx msg
txColumn vc { label, accessor, onClick } =
    Table.veryCustomColumn
        { name = label
        , viewData =
            \data ->
                SidePanelComponents.sidePanelListIdentifierCellWithAttributes
                    SidePanelComponents.sidePanelListIdentifierCellAttributes
                    { root =
                        { copyIconInstance =
                            accessor data |> copyIconPathfinder vc
                        , identifier =
                            accessor data
                                |> truncateLongIdentifierWithLengths 8 4
                        }
                    }
                    |> List.singleton
                    |> wrapCell onClick data

        --, sorter = Table.increasingOrDecreasingBy accessor
        , sorter = Table.unsortable
        }
