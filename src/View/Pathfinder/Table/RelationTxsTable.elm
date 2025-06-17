module View.Pathfinder.Table.RelationTxsTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
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
import Msg.Pathfinder.RelationDetails exposing (Msg(..))
import RecordSetter as Rs
import Table
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Checkbox
import Util.View exposing (copyIconPathfinder, truncateLongIdentifierWithLengths)
import View.Pathfinder.PagedTable as PT exposing (addTHeadOverwrite, alignColumnHeader, customizations)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, wrapCell)


type alias GenericTx =
    { network : String
    , txHash : String
    , id : String
    , timestamp : Int
    , value : Api.Data.Values
    , asset : String
    }


toGerneric : Bool -> Api.Data.Link -> GenericTx
toGerneric isA2b x =
    case x of
        Api.Data.LinkLinkUtxo y ->
            let
                value =
                    if isA2b then
                        y.outputValue

                    else
                        y.inputValue
            in
            GenericTx y.currency y.txHash y.txHash y.timestamp value y.currency

        Api.Data.LinkTxAccount y ->
            GenericTx y.network y.txHash y.identifier y.timestamp y.value y.currency


getId : GenericTx -> Id
getId { network, id } =
    Id.init network id


type alias RelationTxsTableConfig =
    { isA2b : Bool
    , addressId : Id
    , isChecked : Id -> Bool
    , allChecked : Bool
    }


config : Styles -> View.Config -> RelationTxsTableConfig -> Table.Config Api.Data.Link Msg
config styles vc { isA2b, addressId, isChecked, allChecked } =
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
                , msg = UserClickedAllTxCheckboxInTable isA2b
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
                          ]
                            |> css
                        ]
                        [ th [] [ td [] [ addAllCheckbox ] ] ]
                )
                c.thead

        cc =
            c |> Rs.s_thead newTheadWithCheckbox
    in
    Table.customConfig
        { toId = toGerneric isA2b >> getId >> Id.toString
        , toMsg = \_ -> NoOp
        , columns =
            [ PT.checkboxColumn vc
                { isChecked = toGerneric isA2b >> getId >> isChecked
                , onClick = UserClickedTxCheckboxInTable
                , readonly = \_ -> False
                }
            , PT.timestampDateMultiRowColumn vc
                "Timestamp"
                (toGerneric isA2b >> .timestamp)
            , txColumn vc
                { label = "Hash"
                , accessor = toGerneric isA2b >> .txHash
                , onClick = Just (toGerneric isA2b >> getId >> UserClickedTx)
                }
            , PT.debitCreditColumn
                (\_ -> True)
                vc
                (toGerneric isA2b >> .asset >> asset network)
                "Value"
                (toGerneric isA2b >> .value)
            ]
        , customizations = cc
        }


txColumn : View.Config -> ColumnConfig Api.Data.Link msg -> Table.Column Api.Data.Link msg
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
