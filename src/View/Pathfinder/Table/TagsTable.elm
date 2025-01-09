module View.Pathfinder.Table.TagsTable exposing (config, styles)

import Api.Data
import Config.View as View exposing (getConceptName)
import Css
import Css.Table
import Html.Styled exposing (a, span, text)
import Html.Styled.Attributes exposing (css, href, target)
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
import Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)
import Util.Pathfinder.TagSummary exposing (exchangeCategory)
import Util.View exposing (none)
import View.Graph.Table exposing (customizations)
import View.Locale as Locale


tagId : Api.Data.AddressTag -> String
tagId t =
    String.join "|" [ t.address, t.label, t.currency, t.tagpackUri |> Maybe.withDefault "-" ]


type alias CellConfig =
    { label : String, subLabel : Maybe String }


type alias LinkCellConfig =
    { link : Maybe String, label : String, subLabel : Maybe String }


type TagIcon
    = Exchange
    | None


type Cell
    = DefaultCell CellConfig
    | LinkCell LinkCellConfig
    | IconCell TagIcon
    | LabelCell CellConfig TagIcon
    | InfoCell CellConfig String String


linkCellStyle : List Css.Style
linkCellStyle =
    TagsComponents.tagRowCellLabel_details.styles ++ [ Css.property "color" Colors.blue400, Css.textDecoration Css.none ]


cell : View.Config -> Cell -> Table.HtmlDetails Msg
cell _ c =
    let
        attrs =
            TagsComponents.tagRowCellAttributes
                |> Rs.s_tagRowCell ([ Css.maxWidth (Css.px 300), Css.height Css.auto |> Css.important, Css.minHeight (Css.px TagsComponents.tagRowCell_details.height), Css.paddingLeft Css.zero |> Css.important ] |> css |> List.singleton)
                |> Rs.s_iconText ([ Css.height Css.auto |> Css.important, Css.minHeight (Css.px TagsComponents.tagRowCellIconText_details.height) ] |> css |> List.singleton)
                |> Rs.s_category ([ Css.whiteSpace Css.normal |> Css.important, Css.overflowWrap Css.breakWord ] |> css |> List.singleton)

        -- to allow wrapping and growing of line
        defaultData cc tagIcon actionIcon =
            { tagRowCell =
                { infoIconInstance = actionIcon |> Maybe.withDefault none
                , tagIconVisible = tagIcon /= Nothing
                , infoIconVisible = actionIcon /= Nothing
                , labelText = cc.label
                , subLabelVisible = cc.subLabel /= Nothing
                , subLabelText = cc.subLabel |> Maybe.withDefault ""
                , tagIconInstance = none
                }
            }
    in
    (case c of
        DefaultCell cc ->
            TagsComponents.tagRowCellWithAttributes
                attrs
                (defaultData cc Nothing Nothing)

        IconCell ti ->
            case ti of
                Exchange ->
                    TagsComponents.tagRowIconCellWithAttributes
                        (TagsComponents.tagRowIconCellAttributes
                            |> Rs.s_tagRowIconCell
                                [ css
                                    [ Css.verticalAlign Css.top
                                    ]
                                ]
                        )
                        { tagRowIconCell =
                            { iconInstance = Icons.iconsExchangeSnoPadding {}
                            }
                        }

                None ->
                    none

        LabelCell cc _ ->
            TagsComponents.tagRowCellWithAttributes
                attrs
                (defaultData cc Nothing Nothing)

        LinkCell cc ->
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
            in
            TagsComponents.tagRowCellWithInstances
                (attrs |> Rs.s_category [ [ Css.property "color" Colors.blue400 |> Css.important ] |> css ])
                (TagsComponents.tagRowCellInstances |> Rs.s_label linkBody)
                (defaultData cc Nothing linkBodyIcon)

        InfoCell cc titletext cellid ->
            let
                ttConfig =
                    { anchorId = cellid, text = titletext }

                icon =
                    span
                        [ onMouseOver (Msg.Pathfinder.ShowTextTooltip ttConfig |> PathfinderMsg)
                        , onMouseOut (Msg.Pathfinder.CloseTextTooltip ttConfig |> PathfinderMsg)
                        , Html.Styled.Attributes.id cellid
                        ]
                        [ Icons.iconsInfoSnoPaddingWithAttributes
                            (Icons.iconsInfoSnoPaddingAttributes
                                |> Rs.s_shape
                                    [ Css.property "fill-rule" "evenodd"
                                        |> List.singleton
                                        |> css
                                    ]
                            )
                            {}
                        ]
            in
            TagsComponents.tagRowCellWithInstances
                attrs
                TagsComponents.tagRowCellInstances
                (defaultData cc Nothing (Just icon))
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

                    icon =
                        if Set.member exchangeCategory conceptss then
                            Exchange

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

                    conceptss =
                        Set.fromList concepts

                    icon =
                        if Set.member exchangeCategory conceptss then
                            Exchange

                        else
                            None
                in
                cell vc
                    (LabelCell
                        { label = data.label
                        , subLabel =
                            Just
                                (concepts
                                    |> List.map
                                        (\x ->
                                            getConceptName vc (Just x)
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

                    conf =
                        case r of
                            High ->
                                "High confidence"

                            Medium ->
                                "Medium confidence"

                            Low ->
                                "Low confidence"

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

                                _ ->
                                    ""
                            )

                    titleTextWithClusterAddition =
                        if inheritedFromCluster then
                            titleText ++ " " ++ Locale.string vc.locale "Note: This tag was inherited from the cluster level."

                        else
                            titleText
                in
                cell vc
                    (InfoCell
                        { label = Locale.string vc.locale (data.tagType |> String.Extra.toTitleCase)
                        , subLabel = Just (Locale.string vc.locale conf)
                        }
                        titleTextWithClusterAddition
                        (tagId data ++ "_tag_row")
                    )
        , sorter = Table.increasingOrDecreasingBy (\data -> data.confidenceLevel |> Maybe.withDefault 0)
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
                cell vc (LinkCell { label = truncatedSource, link = link, subLabel = Just (Util.View.truncate 30 data.tagpackCreator) })
        , sorter = Table.unsortable
        }


lastModColumn : View.Config -> Table.Column Api.Data.AddressTag Msg
lastModColumn vc =
    Table.veryCustomColumn
        { name = Locale.string vc.locale "Last Modified"
        , viewData =
            \data ->
                let
                    ( date, t ) =
                        data.lastmod |> Maybe.map (\d -> ( Locale.timestampDateUniform vc.locale d, Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset d )) |> Maybe.withDefault ( "-", "-" )
                in
                cell vc (DefaultCell { label = date, subLabel = Just t })
        , sorter = Table.increasingOrDecreasingBy (\data -> data.lastmod |> Maybe.withDefault 0)
        }


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


config : View.Config -> Table.Config Api.Data.AddressTag Msg
config vc =
    Table.customConfig
        { toId = tagId
        , toMsg = TagsListDialogTableUpdateMsg
        , columns =
            [ iconColumn vc
            , labelColumn vc
            , typeColumn vc
            , sourceColumn vc
            , lastModColumn vc
            ]
        , customizations =
            customizations styles vc
        }
