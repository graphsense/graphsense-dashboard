module View.Pathfinder.Table.IoTable exposing (IoColumnConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Dict
import Html.Styled exposing (span)
import Html.Styled.Attributes exposing (css, title)
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (ioToId)
import Msg.Pathfinder exposing (IoDirection, Msg(..), TxDetailsMsg(..))
import RecordSetter as Rs
import Table
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags, isExchangeNode)
import Util.View exposing (copyIconPathfinder, loadingSpinner, none, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (customizations)
import View.Locale as Locale
import View.Pathfinder.PagedTable exposing (alignColumnHeader)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, wrapCell)


type alias IoColumnConfig =
    { network : String
    , hasTags : Id -> HavingTags
    , isChange : Api.Data.TxValue -> Bool
    }


config : Styles -> View.Config -> IoDirection -> (Id -> Bool) -> IoColumnConfig -> Table.Config Api.Data.TxValue Msg
config styles vc ioDirection isCheckedFn ioColumnConfig =
    let
        rightAlignedColumns =
            Dict.fromList [ ( "Value", View.Pathfinder.PagedTable.RightAligned ) ]

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

        network =
            ioColumnConfig.network
    in
    Table.customConfig
        { toId = .address >> String.concat
        , toMsg = TableMsg ioDirection >> TxDetailsMsg
        , columns =
            [ PT.checkboxColumn vc
                { isChecked =
                    ioToId network
                        >> Maybe.map isCheckedFn
                        >> Maybe.withDefault False
                , onClick =
                    ioToId network >> Maybe.map UserClickedAddressCheckboxInTable >> Maybe.withDefault NoOp
                }
            , ioColumn vc
                { label = "Address"
                , accessor = .address >> String.join ","
                , onClick = Just (ioToId network >> Maybe.map UserClickedAddress >> Maybe.withDefault NoOp)
                }
                ioColumnConfig
            , PT.sortableDebitCreditColumn
                (.value >> .value >> (>=) 0)
                vc
                (\_ -> assetFromBase network)
                "Value"
                .value
            ]
        , customizations = customizations styles_ vc |> alignColumnHeader styles_ vc rightAlignedColumns
        }


ioColumn : View.Config -> ColumnConfig Api.Data.TxValue msg -> IoColumnConfig -> Table.Column Api.Data.TxValue msg
ioColumn vc { label, accessor, onClick } { network, hasTags, isChange } =
    let
        exchangeIcon =
            Icons.iconsExchangeSWithAttributes
                (Icons.iconsExchangeSAttributes
                    |> Rs.s_iconsExchangeS
                        [ Locale.string vc.locale "is an exchange"
                            |> title
                        ]
                )
                {}

        tagIcon =
            Icons.iconsTagSWithAttributes
                (Icons.iconsTagSAttributes
                    |> Rs.s_iconsTagS
                        [ Locale.string vc.locale "has tags"
                            |> title
                        ]
                )
                {}

        loadingIcon =
            span
                [ Locale.string vc.locale "Loading tags" |> title
                , css
                    [ Css.px 4
                        |> Css.left
                    , Css.px 5
                        |> Css.top
                    , Css.position Css.absolute
                    ]
                ]
                [ loadingSpinner vc (\_ -> [])
                ]

        hasTags_ =
            ioToId network
                >> Maybe.map hasTags
                >> Maybe.withDefault NoTags
    in
    Table.veryCustomColumn
        { name = label
        , viewData =
            \data ->
                SidePanelComponents.sidePanelIoListIdentifierCellWithAttributes
                    SidePanelComponents.sidePanelIoListIdentifierCellAttributes
                    { sidePanelIoListIdentifierCell =
                        { position1Instance =
                            case hasTags_ data of
                                LoadingTags ->
                                    loadingIcon

                                HasExchangeTagOnly ->
                                    exchangeIcon

                                HasTags _ ->
                                    tagIcon

                                NoTags ->
                                    none

                                HasTagSummary ts ->
                                    if hasOnlyExchangeTags ts then
                                        exchangeIcon

                                    else
                                        tagIcon
                        , position2Instance =
                            case hasTags_ data of
                                HasTagSummary ts ->
                                    if isExchangeNode ts && not (hasOnlyExchangeTags ts) then
                                        exchangeIcon

                                    else
                                        none

                                HasTags True ->
                                    exchangeIcon

                                _ ->
                                    none
                        , changeVisible = isChange data
                        }
                    , changeTag = { text = Locale.string vc.locale "change" }
                    , sidePanelListIdentifierCell =
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
