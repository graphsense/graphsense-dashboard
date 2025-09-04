module View.Pathfinder.Table.IoTable exposing (IoColumnConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Pathfinder as PCSS
import Css.Table exposing (Styles)
import Css.View
import Html.Styled exposing (span, td, th)
import Html.Styled.Attributes exposing (css, title)
import Model.Currency exposing (assetFromBase)
import Model.Direction
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.IoTable exposing (titleValue)
import Model.Pathfinder.Tx exposing (ioToId)
import Msg.Pathfinder exposing (IoDirection(..), Msg(..), TxDetailsMsg(..))
import RecordSetter as Rs
import Table
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Checkbox
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags, isExchangeNode)
import Util.View exposing (copyIconPathfinder, loadingSpinner, none, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (customizations)
import View.Locale as Locale
import View.Pathfinder.PagedTable exposing (addTHeadOverwrite)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, addHeaderAttributes, wrapCell)


type alias IoColumnConfig =
    { network : String
    , hasTags : Id -> HavingTags
    , isChange : Api.Data.TxValue -> Bool
    }


config : Styles -> View.Config -> IoDirection -> (Id -> Bool) -> Bool -> IoColumnConfig -> Table.Config Api.Data.TxValue Msg
config styles vc ioDirection isCheckedFn allChecked ioColumnConfig =
    let
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

        c =
            customizations styles_ vc
                |> addHeaderAttributes styles_ vc titleValue [ css [ Css.textAlign Css.right ] ]

        addAllCheckbox =
            Util.Checkbox.checkbox
                { state = Util.Checkbox.stateFromBool allChecked
                , size = Util.Checkbox.smallSize
                , msg =
                    UserClickedAllAddressCheckboxInTable
                        (case ioDirection of
                            Inputs ->
                                Model.Direction.Outgoing

                            Outputs ->
                                Model.Direction.Incoming
                        )
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
                , readonly = \_ -> False
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
        , customizations = cc
        }


ioColumn : View.Config -> ColumnConfig Api.Data.TxValue msg -> IoColumnConfig -> Table.Column Api.Data.TxValue msg
ioColumn vc { label, accessor, onClick } { network, hasTags, isChange } =
    let
        exchangeIcon =
            Icons.iconsExchangeSWithAttributes
                (Icons.iconsExchangeSAttributes
                    |> Rs.s_root
                        [ Locale.string vc.locale "is an exchange"
                            |> title
                        ]
                )
                {}

        tagIcon =
            Icons.iconsTagSWithAttributes
                (Icons.iconsTagSAttributes
                    |> Rs.s_root
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
                [ loadingSpinner vc Css.View.loadingSpinner
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
                    { root =
                        { position1Instance =
                            let
                                withTagSummary ts =
                                    if hasOnlyExchangeTags ts then
                                        exchangeIcon

                                    else
                                        tagIcon
                            in
                            case hasTags_ data of
                                LoadingTags ->
                                    loadingIcon

                                HasExchangeTagOnly ->
                                    exchangeIcon

                                HasTags _ ->
                                    tagIcon

                                NoTags ->
                                    none

                                NoTagsWithoutCluster ->
                                    none

                                HasTagSummaryWithCluster ts ->
                                    withTagSummary ts

                                HasTagSummaryWithoutCluster _ ->
                                    none

                                HasTagSummaryOnlyWithCluster ts ->
                                    withTagSummary ts

                                HasTagSummaries { withCluster } ->
                                    withTagSummary withCluster
                        , position2Instance =
                            let
                                withTagSummary ts =
                                    if isExchangeNode ts && not (hasOnlyExchangeTags ts) then
                                        exchangeIcon

                                    else
                                        none
                            in
                            case hasTags_ data of
                                HasTagSummaryWithCluster ts ->
                                    withTagSummary ts

                                HasTagSummaryWithoutCluster _ ->
                                    none

                                HasTagSummaries { withCluster } ->
                                    withTagSummary withCluster

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
