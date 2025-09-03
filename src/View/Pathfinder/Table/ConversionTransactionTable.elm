module View.Pathfinder.Table.ConversionTransactionTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Dict
import Html.Styled.Attributes exposing (css)
import Init.Pathfinder.Id as Id
import Model.Currency as Currency
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.ConversionDetails as ConversionDetails
import RecordSetter as Rs
import Table
import Theme.Colors as Colors
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.View exposing (copyIconPathfinder, truncateLongIdentifierWithLengths)
import View.Pathfinder.PagedTable as PT exposing (alignColumnHeader, customizations)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, wrapCell)


type alias GenericTx =
    { network : String
    , txHash : String
    , id : String
    , timestamp : Int
    , value : Api.Data.Values
    , asset : String
    }


getAsset : GenericTx -> Currency.AssetIdentifier
getAsset { network, asset } =
    Currency.asset network asset


toGerneric : Api.Data.Tx -> GenericTx
toGerneric x =
    case x of
        Api.Data.TxTxUtxo y ->
            GenericTx y.currency y.txHash y.txHash y.timestamp y.totalOutput y.currency

        Api.Data.TxTxAccount y ->
            GenericTx y.network y.txHash y.identifier y.timestamp y.value y.currency


getId : GenericTx -> Id
getId { network, id } =
    Id.init network id


config : Styles -> View.Config -> ( Id, Id ) -> (Id -> Bool) -> Table.Config Api.Data.Tx Msg
config styles vc tId isCheckedFn =
    let
        rightAlignedColumns =
            Dict.fromList [ ( "Value", PT.RightAligned ) ]

        rowBaseAttrs =
            [ Css.height (Css.px 52)
            , Css.property "border" ("1px solid " ++ Colors.grey50)
            ]

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
    in
    Table.customConfig
        { toId = toGerneric >> getId >> Id.toString
        , columns =
            [ PT.checkboxColumn vc
                { isChecked = toGerneric >> getId >> isCheckedFn
                , onClick = toGerneric >> getId >> ConversionDetails.UserClickedTxCheckboxInTable >> ConversionDetailsMsg tId
                , readonly = \_ -> False
                }
            , PT.timestampDateMultiRowColumn vc
                "Timestamp"
                (toGerneric >> .timestamp)
            , txColumn vc
                { label = "Hash"
                , accessor = toGerneric >> .txHash
                , onClick = Nothing
                }
            , PT.valueColumnWithOptions
                { sortable = False
                , hideCode = False
                , colorFlowDirection = False
                , isOutgoingFn = always False
                }
                vc
                (toGerneric >> getAsset)
                "Value"
                (toGerneric >> .value)
            ]
        , customizations =
            customizations vc
                |> Rs.s_rowAttrs
                    (\_ ->
                        rowBaseAttrs
                            |> css
                            |> List.singleton
                    )
                |> alignColumnHeader styles_ vc rightAlignedColumns
        , toMsg = always NoOp
        }


txColumn : View.Config -> ColumnConfig Api.Data.Tx msg -> Table.Column Api.Data.Tx msg
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
        , sorter = Table.unsortable
        }
