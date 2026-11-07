module View.Pathfinder.Table.ConversionTransactionTable exposing (config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Html.Styled exposing (Html, a, span)
import Html.Styled.Attributes exposing (css, href, rel, target, title)
import Init.Pathfinder.Id as Id
import Model.Currency as Currency
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.ConversionDetails as ConversionDetails
import RecordSetter as Rs
import Table
import Theme.Colors as Colors
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.ExternalLinks exposing (getBlockExplorerTransactionLinks)
import Util.View exposing (copyIconPathfinder, none, testId, truncateLongIdentifierWithLengths)
import View.Pathfinder.PagedTable exposing (customizations)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, addHeaderAttributes, applyHeaderCustomizations, initCustomHeaders, wrapCell)


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
                ""
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
            initCustomHeaders
                |> addHeaderAttributes "Value" [ css [ Css.textAlign Css.right ] ]
                |> flip (applyHeaderCustomizations styles_ vc) (customizations vc)
                |> Rs.s_rowAttrs
                    (\_ ->
                        rowBaseAttrs
                            |> css
                            |> List.singleton
                    )
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
                            span [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                                [ accessor data |> copyIconPathfinder vc
                                , toGerneric data |> legExplorerLink
                                ]
                        , identifier =
                            accessor data
                                |> truncateLongIdentifierWithLengths 8 4
                        }
                    }
                    |> List.singleton
                    |> wrapCell onClick data
        , sorter = Table.unsortable
        }


{-| The leg in its chain's first explorer, as in the tx id's chevron menu. It
opens in a new tab, only on click, and the explorer is not told where the click
came from.
-}
legExplorerLink : GenericTx -> Html msg
legExplorerLink { network, id } =
    case getBlockExplorerTransactionLinks network id |> List.head of
        Just ( url, name ) ->
            a
                [ href url
                , target "_blank"
                , rel "noopener noreferrer"
                , title name
                , testId "leg-explorer-link"
                , css
                    [ Css.displayFlex
                    , Css.alignItems Css.center

                    -- into the copy icon's own padding, and as much again
                    -- after: the legs table already fills the panel, so the
                    -- icon must not widen the cell, nor touch a long value
                    , Css.marginLeft (Css.px -3)
                    , Css.marginRight (Css.px 3)
                    ]
                ]
                [ HIcons.iconsGoToSnoPadding {} ]

        Nothing ->
            none
