module View.Pathfinder.Table.IoTable exposing (IoColumnConfig, config)

import Api.Data
import Basics.Extra exposing (flip)
import Char
import Components.InfiniteTable as InfiniteTable
import Components.Tooltip as Tooltip
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Css.View
import Html.Styled exposing (span)
import Html.Styled.Attributes exposing (css, style, title)
import Model.Currency exposing (assetFromBase)
import Model.Direction
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.IoTable exposing (titleValue)
import Model.Pathfinder.Tx exposing (ioToId)
import Msg.Pathfinder.TxDetails exposing (IoDirection(..), Msg(..))
import RecordSetter as Rs
import Sha256
import Table
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags, isExchangeNode)
import Util.Tooltip
import Util.TooltipType as TooltipType
import Util.View exposing (copyIconPathfinder, loadingSpinner, none, truncateLongIdentifierWithLengths)
import View.Locale as Locale
import View.Pathfinder.InfiniteTable as PathfinderInfiniteTable
import View.Pathfinder.PagedTable exposing (customizations)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, addHeaderAttributes, applyHeaderCustomizations, initCustomHeaders, setHeaderCheckbox, wrapCell)


type alias IoColumnConfig =
    { network : String
    , hasTags : Id -> HavingTags
    , getChangeInfo : Api.Data.TxValue -> Maybe { confidence : Float, heuristics : List String }
    }


config : Styles -> View.Config -> IoDirection -> (Id -> Bool) -> Bool -> IoColumnConfig -> InfiniteTable.TableConfig Api.Data.TxValue Msg
config styles vc ioDirection isCheckedFn allChecked ioColumnConfig =
    let
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

        checkboxTitle =
            "checkbox"

        cc =
            initCustomHeaders
                |> addHeaderAttributes titleValue [ css [ Css.textAlign Css.right ] ]
                |> setHeaderCheckbox checkboxTitle allChecked allCheckedMsg
                |> flip (applyHeaderCustomizations styles_ vc) (customizations vc)

        direction =
            case ioDirection of
                Inputs ->
                    Model.Direction.Outgoing

                Outputs ->
                    Model.Direction.Incoming

        allCheckedMsg =
            UserClickedAllIoTableCheckboxes direction

        network =
            ioColumnConfig.network
    in
    { toId = .address >> String.concat
    , columns =
        [ PT.checkboxColumn vc
            checkboxTitle
            { isChecked =
                ioToId network
                    >> Maybe.map isCheckedFn
                    >> Maybe.withDefault False
            , onClick =
                ioToId network
                    >> Maybe.map UserClickedIoTableCheckbox
                    >> Maybe.withDefault NoOp
            , readonly = \_ -> False
            }
        , ioColumn vc
            { label = "Address"
            , accessor = .address >> String.join ","
            , onClick =
                Just (ioToId network >> Maybe.map UserClickedIoTableAddress >> Maybe.withDefault NoOp)
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
    , tag = IoTableMsg ioDirection
    , loadingPlaceholderAbove = PathfinderInfiniteTable.loadingPlaceholderAbove vc
    , loadingPlaceholderBelow = PathfinderInfiniteTable.loadingPlaceholderBelow vc
    }


ioColumn : View.Config -> ColumnConfig Api.Data.TxValue Msg -> IoColumnConfig -> Table.Column Api.Data.TxValue Msg
ioColumn vc { label, accessor, onClick } { network, hasTags, getChangeInfo } =
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

        humanReadableHeuristic : String -> String
        humanReadableHeuristic heuristic =
            case heuristic of
                "one_time_change" ->
                    "One-Time Change"

                "direct_change" ->
                    "Direct Change"

                "multi_input_change" ->
                    "Multi-Input Change"

                "all" ->
                    "All"

                _ ->
                    heuristic
                        |> String.split "_"
                        |> List.map
                            (\word ->
                                case String.uncons word of
                                    Just ( firstChar, rest ) ->
                                        String.fromChar (Char.toUpper firstChar) ++ rest

                                    Nothing ->
                                        word
                            )
                        |> String.join " "

        changeBadgeConfigFromInfo rowDomScope maybeChangeInfo =
            case maybeChangeInfo of
                Just changeInfo ->
                    let
                        confidence =
                            changeInfo.confidence

                        confidenceRange =
                            getConfidenceRangeFromFloat confidence

                        ( backgroundColor, borderColor ) =
                            case confidenceRange of
                                High ->
                                    ( Colors.green20, Colors.annotation1 )

                                Medium ->
                                    ( Colors.tagsMediumBg, Colors.tagsMedium )

                                Low ->
                                    ( Colors.tagsLowBg, Colors.tagsLow )

                        confidencePercent =
                            round (confidence * 100)

                        heuristics =
                            changeInfo.heuristics
                                |> List.map humanReadableHeuristic
                                |> List.filter (String.isEmpty >> not)
                    in
                    { change =
                        []
                    , changeTag =
                        [ style "background-color" backgroundColor
                        , style "border-color" borderColor
                        , style "border-style" "solid"
                        ]
                    , isVisible = True
                    , tooltip =
                        Just
                            { domId =
                                "txdetails_change_"
                                    ++ rowDomScope
                                    ++ "_"
                                    ++ String.fromInt confidencePercent
                                    ++ "_"
                                    ++ (String.join "_" heuristics |> String.replace " " "_")
                            , confidence = confidence
                            , heuristics = heuristics
                            }
                    }

                Nothing ->
                    { change = []
                    , changeTag = []
                    , isVisible = False
                    , tooltip = Nothing
                    }
    in
    Table.veryCustomColumn
        { name = label
        , viewData =
            \data ->
                let
                    rowDomScope =
                        accessor data
                            |> Sha256.sha256

                    changeBadgeConfig =
                        getChangeInfo data
                            |> changeBadgeConfigFromInfo rowDomScope

                    attributes =
                        SidePanelComponents.sidePanelIoListIdentifierCellAttributes

                    changeTooltipAttrs =
                        case changeBadgeConfig.tooltip of
                            Just tt ->
                                TooltipType.ChangeHeuristics { confidence = tt.confidence, heuristics = tt.heuristics }
                                    |> Tooltip.attributes tt.domId (Util.Tooltip.tooltipConfig vc TooltipMsg)

                            Nothing ->
                                []
                in
                SidePanelComponents.sidePanelIoListIdentifierCellWithAttributes
                    { attributes
                        | change = changeBadgeConfig.change
                        , changeTag = changeTooltipAttrs ++ changeBadgeConfig.changeTag
                    }
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

                                HasClusterTagsOnlyButNoDirect ->
                                    none
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
                        , changeVisible = changeBadgeConfig.isVisible
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
