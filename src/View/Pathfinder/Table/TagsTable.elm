module View.Pathfinder.Table.TagsTable exposing (config, styles)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.Tooltip as Tooltip
import Config.View as View exposing (getConceptName)
import Css
import Css.Pathfinder exposing (fullWidth)
import Css.Table
import Html.Styled exposing (a, div, span, text)
import Html.Styled.Attributes exposing (css, href, target, title)
import Html.Styled.Events exposing (onMouseOut, onMouseOver)
import Model exposing (Msg(..))
import Msg.Pathfinder
import RecordSetter as Rs
import Set
import String.Extra
import Table
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Theme.Html.TagsComponents as TagsComponents
import Url
import Util.Data as Data
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.Pathfinder.TagSummary exposing (exchangeCategory)
import Util.Tooltip as Util
import Util.TooltipType
import Util.View exposing (none)
import View.Graph.Table exposing (customizations)
import View.Locale as Locale
import View.Pathfinder.InfiniteTable as ViewInfiniteTable
import View.Pathfinder.Table.Columns exposing (addHeaderAttributes, applyHeaderCustomizations, initCustomHeaders)


tagId : Api.Data.AddressTag -> String
tagId t =
    String.join "|" [ t.address, t.label, t.currency, t.tagpackUri |> Maybe.withDefault "-" ]



-- tags without label are not valid in our backend
-- we remove labels if the user plan does not contain those tags.


isProprietaryTag : Api.Data.AddressTag -> Bool
isProprietaryTag t =
    String.isEmpty t.label && not t.tagpackIsPublic


type alias CellConfig =
    { label : String, subLabel : Maybe String }


type alias ConfidenceCellConfig =
    { label : String, confidence : ConfidenceRange, titletext : String, cellid : String }


type alias LinkCellConfig =
    { link : Maybe String, label : String, subLabel : Maybe String }


type TagIcon
    = Exchange
    | DirectTag
    | IndirectTag
    | None


type Cell
    = LastModCell CellConfig
    | SourceCell LinkCellConfig
    | IconCell TagIcon
    | LabelCell CellConfig TagIcon
    | TypeCell ConfidenceCellConfig


linkCellStyle : List Css.Style
linkCellStyle =
    TagsComponents.tagRowCellLabel_details.styles ++ [ Css.property "color" Colors.blue400, Css.textDecoration Css.none ]


cell : View.Config -> Cell -> Table.HtmlDetails Msg
cell vc c =
    let
        cellBase =
            [ Css.height (Css.px TagsComponents.tagRowCell_details.height)
            ]

        cellWMinWidth =
            cellBase ++ [ Css.minWidth (Css.px 150) ]

        cellWWidth =
            [ Css.marginRight (Css.px 15), Css.maxWidth (Css.px 300) |> Css.important ] ++ cellWMinWidth

        attrs showTooltip cc =
            TagsComponents.tagRowCellAttributes
                |> Rs.s_iconText
                    ([ Css.height (Css.px TagsComponents.tagRowCellIconText_details.height)
                     ]
                        |> css
                        |> List.singleton
                    )
                |> Rs.s_label
                    (if showTooltip then
                        [ title cc.label ]

                     else
                        []
                    )
                |> Rs.s_category
                    (if showTooltip then
                        [ cc.subLabel |> Maybe.withDefault "" |> title ]

                     else
                        []
                    )

        -- to allow wrapping and growing of line
        defaultData cc tagIcon actionIcon =
            { root =
                { infoIconInstance = actionIcon |> Maybe.withDefault none
                , tagIconVisible = tagIcon /= Nothing
                , infoIconVisible = actionIcon /= Nothing
                , labelText = cc.label |> Util.View.truncate 60
                , subLabelVisible = cc.subLabel /= Nothing
                , subLabelText = cc.subLabel |> Maybe.withDefault "" |> Util.View.truncate 60
                , tagIconInstance = none
                }
            }
    in
    (case c of
        LastModCell cc ->
            TagsComponents.tagRowCellWithAttributes
                (attrs False cc
                    |> Rs.s_root (cellBase ++ [ Css.alignItems Css.end |> Css.important ] |> css |> List.singleton)
                )
                (defaultData cc Nothing Nothing)

        IconCell ti ->
            let
                iconPlaceholder =
                    Html.Styled.div
                        [ css
                            [ Css.width (Css.px TagsComponents.tagRowIconCell_details.width)
                            , Css.height (Css.px TagsComponents.tagRowIconCell_details.height)
                            ]
                        ]
                        []
            in
            case ti of
                Exchange ->
                    TagsComponents.tagRowIconCellWithAttributes
                        (TagsComponents.tagRowIconCellAttributes
                            |> Rs.s_root
                                [ css
                                    [ Css.verticalAlign Css.top
                                    ]
                                ]
                        )
                        { root =
                            { iconInstance = Icons.iconsExchangeSnoPadding {}
                            }
                        }

                DirectTag ->
                    iconPlaceholder

                IndirectTag ->
                    iconPlaceholder

                None ->
                    iconPlaceholder

        LabelCell cc _ ->
            TagsComponents.tagRowCellWithAttributes
                (attrs True cc |> Rs.s_root (cellWWidth |> css |> List.singleton))
                (defaultData cc Nothing Nothing)

        SourceCell cc ->
            let
                getLink url body =
                    a [ href url, target "blank", linkCellStyle |> css ] (body |> List.singleton)

                linkBody =
                    cc.link |> Maybe.map (\x -> getLink x (text cc.label))

                linkIcon =
                    Icons.iconsGoToSnoPaddingWithAttributes
                        (Icons.iconsGoToSnoPaddingAttributes
                            |> Rs.s_goTo ([ Css.property "fill" Colors.blue400 |> Css.important ] |> css |> List.singleton)
                        )
                        {}

                linkBodyIcon =
                    cc.link |> Maybe.map (\x -> getLink x linkIcon)

                subText =
                    cc.subLabel |> Maybe.withDefault ""

                sub =
                    span
                        [ [ Css.property "color" Colors.blue400 |> Css.important ] |> css
                        , title subText
                        ]
                        [ text subText ]
            in
            TagsComponents.tagRowCellWithInstances
                (attrs False cc |> Rs.s_root (cellWWidth |> css |> List.singleton))
                (TagsComponents.tagRowCellInstances
                    |> Rs.s_label linkBody
                    |> Rs.s_category (Just sub)
                )
                (defaultData cc Nothing linkBodyIcon)

        TypeCell cc ->
            let
                lbl =
                    case cc.confidence of
                        High ->
                            "high confidence"

                        Medium ->
                            "medium confidence"

                        Low ->
                            "low confidence"

                cl =
                    case cc.confidence of
                        High ->
                            TagsComponents.ConfidenceLevelConfidenceLevelHigh

                        Medium ->
                            TagsComponents.ConfidenceLevelConfidenceLevelMedium

                        Low ->
                            TagsComponents.ConfidenceLevelConfidenceLevelLow

                sub =
                    TagsComponents.confidenceLevel
                        { root =
                            { size = TagsComponents.ConfidenceLevelSizeSmall
                            , confidenceLevel = cl
                            , text = Locale.string vc.locale lbl |> Locale.titleCase vc.locale
                            }
                        }

                cellConfig =
                    { label = cc.label, subLabel = Just "" }

                tooltipConfig =
                    Util.tooltipConfig vc (\tipMsg -> PathfinderMsg (Msg.Pathfinder.TooltipMsg tipMsg))
                        |> Tooltip.withFixed

                icon =
                    Icons.iconsInfoSnoPaddingDevWithAttributes
                        (Icons.iconsInfoSnoPaddingDevAttributes
                            |> Rs.s_root
                                (Util.TooltipType.Text cc.titletext
                                    |> Tooltip.attributes cc.cellid tooltipConfig
                                )
                        )
                        {}
            in
            TagsComponents.tagRowCellWithInstances
                (attrs False cellConfig |> Rs.s_root (cellWMinWidth |> css |> List.singleton))
                (TagsComponents.tagRowCellInstances
                    |> Rs.s_category (Just sub)
                )
                (defaultData cellConfig Nothing (Just icon))
    )
        |> List.singleton
        |> Table.HtmlDetails
            [ css [ Css.verticalAlign Css.top ] ]


iconColumn : View.Config -> Table.Column Api.Data.AddressTag Msg
iconColumn vc =
    Table.veryCustomColumn
        { name = ""
        , viewData =
            \data ->
                let
                    mconcept =
                        data.category |> Maybe.map List.singleton |> Maybe.withDefault []

                    concepts =
                        mconcept ++ (data.concepts |> Maybe.withDefault [])

                    conceptss =
                        Set.fromList concepts

                    inheritedFromCluster =
                        data.inheritedFrom == Just Api.Data.AddressTagInheritedFromCluster

                    icon =
                        if Set.member exchangeCategory conceptss then
                            Exchange

                        else if inheritedFromCluster then
                            IndirectTag

                        else
                            None
                in
                cell vc (IconCell icon)
        , sorter = Table.unsortable
        }


labelColumn : View.Config -> Table.Column Api.Data.AddressTag Msg
labelColumn vc =
    Table.veryCustomColumn
        { name = Locale.string vc.locale "Label"
        , viewData =
            \data ->
                let
                    mconcept =
                        data.category |> Maybe.map List.singleton |> Maybe.withDefault []

                    concepts =
                        mconcept ++ (data.concepts |> Maybe.withDefault [])

                    concepts_w_default =
                        if List.isEmpty concepts then
                            [ "unknown" ]

                        else
                            concepts

                    conceptss =
                        Set.fromList concepts

                    inheritedFromCluster =
                        data.inheritedFrom == Just Api.Data.AddressTagInheritedFromCluster

                    icon =
                        if Set.member exchangeCategory conceptss then
                            Exchange

                        else if inheritedFromCluster then
                            IndirectTag

                        else
                            None
                in
                cell vc
                    (LabelCell
                        { label =
                            if isProprietaryTag data then
                                Locale.string vc.locale "proprietary tag"

                            else
                                data.label
                        , subLabel =
                            Just
                                (concepts_w_default
                                    |> List.map
                                        (\x ->
                                            getConceptName vc x
                                                |> Maybe.withDefault x
                                        )
                                    |> String.join ", "
                                )
                        }
                        icon
                    )
        , sorter = Table.unsortable
        }


typeColumn : View.Config -> Table.Column Api.Data.AddressTag Msg
typeColumn vc =
    Table.veryCustomColumn
        { name = Locale.string vc.locale "Type"
        , viewData =
            \data ->
                let
                    conf_l =
                        (data.confidenceLevel |> Maybe.withDefault 0 |> toFloat) / 100

                    r =
                        getConfidenceRangeFromFloat conf_l

                    inheritedFromCluster =
                        data.inheritedFrom == Just Api.Data.AddressTagInheritedFromCluster

                    titleText =
                        Locale.string vc.locale
                            (case data.tagType of
                                "mention" ->
                                    "mentionTagDescriptionId"

                                "actor" ->
                                    "actorTagDescriptionId"

                                "event" ->
                                    "eventTagDescriptionId"

                                "attribute" ->
                                    "attributeTagDescriptionId"

                                _ ->
                                    ""
                            )

                    titleTextWithClusterAddition =
                        if inheritedFromCluster then
                            titleText ++ " " ++ Locale.string vc.locale "Note-tag-inherited"

                        else
                            titleText
                in
                cell vc
                    (TypeCell
                        { label =
                            data.tagType
                                |> String.Extra.toTitleCase
                                |> Locale.string vc.locale
                                |> Locale.titleCase vc.locale
                        , confidence = r
                        , titletext = titleTextWithClusterAddition
                        , cellid = tagId data ++ "_tag_row"
                        }
                    )
        , sorter = Table.unsortable
        }


sourceColumn : View.Config -> Table.Column Api.Data.AddressTag Msg
sourceColumn vc =
    Table.veryCustomColumn
        { name = Locale.string vc.locale "Source"
        , viewData =
            \data ->
                let
                    url =
                        data.source |> Maybe.withDefault "#"

                    s =
                        url
                            |> String.replace "https://" ""
                            |> String.replace "http://" ""

                    truncatedSource =
                        case String.split "/" s of
                            a :: _ ->
                                a

                            _ ->
                                "link"

                    pUrl =
                        Url.fromString url

                    link =
                        case pUrl of
                            Just _ ->
                                Just url

                            _ ->
                                Nothing
                in
                cell vc (SourceCell { label = truncatedSource, link = link, subLabel = Just (Util.View.truncate 30 data.tagpackCreator) })
        , sorter = Table.unsortable
        }


lastModColumn : View.Config -> Table.Column Api.Data.AddressTag Msg
lastModColumn vc =
    Table.veryCustomColumn
        { name = titleLastModified
        , viewData =
            \data ->
                let
                    ( date, _ ) =
                        data.lastmod
                            |> Maybe.map Data.timestampToPosix
                            |> Maybe.map
                                (\d ->
                                    ( Locale.timestampDateUniform vc.locale d
                                    , Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset d
                                    )
                                )
                            |> Maybe.withDefault ( "-", "-" )
                in
                cell vc (LastModCell { label = date, subLabel = Nothing })
        , sorter = Table.unsortable
        }


titleLastModified : String
titleLastModified =
    "Last modified"


styles : Css.Table.Styles
styles =
    Css.Table.styles
        |> Rs.s_root
            (\_ ->
                [ Css.display Css.block
                , Css.width (Css.pct 100)
                , Css.paddingTop (Css.px 16)
                , Css.verticalAlign Css.top
                ]
            )
        |> Rs.s_headRow
            (\_ ->
                [ Css.height (Css.px 24)
                , Css.textAlign Css.left
                , Css.borderBottom2 (Css.px 1) Css.solid
                , Css.property "border-color" Colors.grey50
                ]
            )
        |> Rs.s_row
            (\_ ->
                [ Css.borderBottom2 (Css.px 1) Css.solid
                , Css.property "border-color" Colors.grey50
                , Css.property "content-visibility" "auto"
                , Css.property "contain-intrinsic-size" "auto 50px"
                ]
            )
        |> Rs.s_headCell
            (\_ ->
                SidePanelComponents.sidePanelListHeadCell_details.styles
                    ++ SidePanelComponents.sidePanelListHeadCellPlaceholder_details.styles
                    ++ [ Css.verticalAlign Css.middle
                       , Css.display Css.tableCell
                       , Css.backgroundColor Css.transparent
                       ]
            )
        |> Rs.s_table
            (\vc -> Css.Table.styles.table vc ++ fullWidth)


config : View.Config -> (InfiniteTable.Msg -> Msg) -> InfiniteTable.TableConfig Api.Data.AddressTag Msg
config vc tag =
    { toId = tagId
    , columns =
        [ iconColumn vc
        , labelColumn vc
        , typeColumn vc
        , sourceColumn vc
        , lastModColumn vc
        ]
    , customizations =
        initCustomHeaders
            |> addHeaderAttributes titleLastModified [ css [ Css.textAlign Css.right ] ]
            |> flip (applyHeaderCustomizations styles vc) (customizations styles vc)
    , tag = tag
    , loadingPlaceholderAbove = ViewInfiniteTable.loadingPlaceholderAbove vc
    , loadingPlaceholderBelow = ViewInfiniteTable.loadingPlaceholderBelow vc
    }
