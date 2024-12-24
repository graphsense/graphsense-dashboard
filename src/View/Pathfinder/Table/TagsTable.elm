module View.Pathfinder.Table.TagsTable exposing (..)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Init.Pathfinder.Id as Id
import Model exposing (Msg(..))
import Model.Currency exposing (asset)
import Model.Pathfinder.Id as Id exposing (Id)
import RecordSetter as Rs
import Set
import Table
import Theme.Html.TagsComponents as TagsComponents
import Util.View exposing (copyIconPathfinder, none, truncateLongIdentifierWithLengths)
import View.Locale as Locale
import View.Pathfinder.PagedTable exposing (alignColumnsRight, customizations)
import View.Pathfinder.Table.Columns as PT exposing (ColumnConfig, wrapCell)


tagId : Api.Data.AddressTag -> String
tagId t =
    String.join "|" [ t.address, t.label, t.currency, t.tagpackUri |> Maybe.withDefault "-" ]


labelColumn : View.Config -> Table.Column Api.Data.AddressTag msg
labelColumn vc =
    Table.veryCustomColumn
        { name = Locale.string vc.locale "Label"
        , viewData =
            \data ->
                let
                    cate =
                        data.category |> Maybe.withDefault "-"
                in
                Table.HtmlDetails
                    []
                    [ TagsComponents.tagRowCell
                        { tagRowCell =
                            { actionIconInstance = none
                            , iconVisible = False
                            , infoVisible = False
                            , labelText = data.label
                            , subLabelTextVisible = True
                            , subLabelText = cate
                            , tagIconInstance = none
                            }
                        }
                    ]
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
                        data.confidenceLevel |> Maybe.withDefault 0

                    conf =
                        if conf_l > 70 then
                            "High confidence"

                        else if conf_l > 40 then
                            "Medium confidence"

                        else
                            "Low confidence"
                in
                Table.HtmlDetails
                    []
                    [ TagsComponents.tagRowCell
                        { tagRowCell =
                            { actionIconInstance = none
                            , iconVisible = False
                            , infoVisible = True
                            , labelText = Locale.string vc.locale "Actor"
                            , subLabelTextVisible = False
                            , subLabelText = Locale.string vc.locale conf
                            , tagIconInstance = none
                            }
                        }
                    ]
        , sorter = Table.unsortable
        }


sourceColumn : View.Config -> Table.Column Api.Data.AddressTag msg
sourceColumn vc =
    Table.veryCustomColumn
        { name = Locale.string vc.locale "Source"
        , viewData =
            \data ->
                let
                    s =
                        data.source |> Maybe.withDefault "-" |> String.replace "https://" ""

                    truncatedSource =
                        String.left 10 s ++ "..." ++ String.right 15 s
                in
                Table.HtmlDetails
                    []
                    [ TagsComponents.tagRowCell
                        { tagRowCell =
                            { actionIconInstance = none
                            , iconVisible = False
                            , infoVisible = True
                            , labelText = truncatedSource
                            , subLabelTextVisible = True
                            , subLabelText = data.tagpackCreator
                            , tagIconInstance = none
                            }
                        }
                    ]
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
                Table.HtmlDetails
                    []
                    [ TagsComponents.tagRowCell
                        { tagRowCell =
                            { actionIconInstance = none
                            , iconVisible = False
                            , infoVisible = True
                            , labelText = date
                            , subLabelTextVisible = True
                            , subLabelText = t
                            , tagIconInstance = none
                            }
                        }
                    ]
        , sorter = Table.unsortable
        }


config : View.Config -> Table.Config Api.Data.AddressTag Msg
config vc =
    let
        styles_ =
            Css.Table.styles
                |> Rs.s_root
                    (Css.Table.styles.root
                        >> flip (++)
                            [ Css.display Css.block
                            , Css.width (Css.pct 100)
                            ]
                    )
    in
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
            customizations vc
                |> alignColumnsRight styles_ vc Set.empty
        }
