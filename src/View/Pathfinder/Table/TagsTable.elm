module View.Pathfinder.Table.TagsTable exposing (config, styles)

import Api.Data
import Config.View as View exposing (getConceptName)
import Css
import Css.Table
import Html.Styled exposing (a, span, text)
import Html.Styled.Attributes exposing (css, href, target, title)
import Model exposing (Msg(..))
import RecordSetter as Rs
import Set
import String.Extra
import Table
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
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
    | LabelCell CellConfig TagIcon
    | InfoCell CellConfig String


linkCellStyle : List Css.Style
linkCellStyle =
    TagsComponents.tagRowCellLabel_details.styles ++ [ Css.property "color" Colors.blue400, Css.textDecoration Css.none ]


cell : View.Config -> Cell -> Table.HtmlDetails msg
cell _ c =
    let
        attrs =
            TagsComponents.tagRowCellAttributes
                |> Rs.s_line ([ Css.display Css.none ] |> css |> List.singleton)
                |> Rs.s_tagRowCell ([ Css.maxWidth (Css.px 200), Css.property "word-wrap" "break-word" ] |> css |> List.singleton)
                |> Rs.s_category ([ Css.property "text-wrap" "wrap" ] |> css |> List.singleton)

        defaultData cc tagIcon actionIcon =
            { tagRowCell =
                { actionIconInstance = actionIcon |> Maybe.withDefault none
                , iconVisible = tagIcon /= Nothing
                , infoVisible = actionIcon /= Nothing
                , labelText = cc.label
                , subLabelTextVisible = cc.subLabel /= Nothing
                , subLabelText = cc.subLabel |> Maybe.withDefault ""
                , tagIconInstance = tagIcon |> Maybe.withDefault none
                }
            }
    in
    (case c of
        DefaultCell cc ->
            TagsComponents.tagRowCellWithAttributes
                attrs
                (defaultData cc Nothing Nothing)

        LabelCell cc ti ->
            let
                icon =
                    case ti of
                        Exchange ->
                            Just (Icons.iconsExchangeSmall {})

                        None ->
                            Nothing
            in
            TagsComponents.tagRowCellWithAttributes
                attrs
                (defaultData cc icon Nothing)

        LinkCell cc ->
            let
                getLink url body =
                    a [ href url, target "blank", linkCellStyle |> css ] (body |> List.singleton)

                linkBody =
                    cc.link |> Maybe.map (\x -> getLink x (text cc.label))

                linkIcon =
                    Icons.iconsGoToSmallWithAttributes
                        (Icons.iconsGoToSmallAttributes
                            |> Rs.s_goTo ([ Css.property "fill" Colors.blue400 |> Css.important ] |> css |> List.singleton)
                        )
                        {}

                linkBodyIcon =
                    cc.link |> Maybe.map (\x -> getLink x linkIcon)
            in
            TagsComponents.tagRowCellWithInstances
                attrs
                (TagsComponents.tagRowCellInstances |> Rs.s_label linkBody)
                (defaultData cc Nothing linkBodyIcon)

        InfoCell cc titletext ->
            let
                icon =
                    span [ title titletext ] [ Icons.iconsInfoSmall {} ]
            in
            TagsComponents.tagRowCellWithInstances
                attrs
                TagsComponents.tagRowCellInstances
                (defaultData cc Nothing (Just icon))
    )
        |> List.singleton
        |> Table.HtmlDetails
            [ [ Css.verticalAlign Css.middle ] |> css ]


labelColumn : View.Config -> Table.Column Api.Data.AddressTag msg
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


typeColumn : View.Config -> Table.Column Api.Data.AddressTag msg
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
                                    "A mention says that this address was mentioned e.g. a website. It might be of relevance, but it depends on the context."

                                "actor" ->
                                    "An actor tag is a statement about the party controlling the address."

                                "event" ->
                                    "An event tag is a statement about an event taking place with relation to this address e.g. if the address was part of a hack."

                                _ ->
                                    ""
                            )

                    titleTextWithClusterAddition =
                        if inheritedFromCluster then
                            titleText ++ " " ++ Locale.string vc.locale "Note: This tag was inherited from the cluster level."

                        else
                            titleText
                in
                cell vc (InfoCell { label = Locale.string vc.locale (data.tagType |> String.Extra.toTitleCase), subLabel = Just (Locale.string vc.locale conf) } titleTextWithClusterAddition)
        , sorter = Table.unsortable
        }


sourceColumn : View.Config -> Table.Column Api.Data.AddressTag msg
sourceColumn vc =
    Table.veryCustomColumn
        { name = Locale.string vc.locale "Source"
        , viewData =
            \data ->
                let
                    url =
                        data.source |> Maybe.withDefault "#"

                    s =
                        url |> String.replace "https://" ""

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


lastModColumn : View.Config -> Table.Column Api.Data.AddressTag msg
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
                , Css.property "border-color" Colors.greyBlue100
                ]
            )
        |> Rs.s_row
            (\_ ->
                [ Css.borderBottom2 (Css.px 1) Css.solid
                , Css.property "border-color" Colors.greyBlue100
                , Css.verticalAlign Css.top
                ]
            )
        |> Rs.s_headCell
            (\_ ->
                (-- TagsComponents.dialogTagsListComponentCellsLayout_details.styles
                 [ Css.display Css.tableCell, Css.verticalAlign Css.top, Css.property "color" Colors.greyBlue500 ]
                )
            )


config : View.Config -> Table.Config Api.Data.AddressTag Msg
config vc =
    Table.customConfig
        { toId = tagId
        , toMsg = \_ -> NoOp
        , columns =
            [ labelColumn vc
            , typeColumn vc
            , sourceColumn vc
            , lastModColumn vc
            ]
        , customizations =
            customizations styles vc
        }
