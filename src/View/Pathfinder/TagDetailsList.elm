module View.Pathfinder.TagDetailsList exposing (view)

import Api.Data
import Components.Table exposing (Table)
import Config.View as View
import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Model exposing (Msg)
import Model.Pathfinder.Id as Id exposing (Id)
import RecordSetter as Rs
import Theme.Html.TagsComponents as TagsComponents
import Util.View exposing (copyIconPathfinder, none, onClickWithStop)
import View.Graph.Table
import View.Locale as Locale
import View.Pathfinder.Table.TagsTable as TagsTable


view : View.Config -> Msg -> Id -> Table Api.Data.AddressTag -> Html Msg
view vc closeMsg id tags =
    let
        fullWidthAttr =
            Css.pct 100 |> Css.width |> List.singleton |> css |> List.singleton

        header =
            TagsComponents.dialogTagHeaderWithAttributes
                (TagsComponents.dialogTagHeaderAttributes
                    |> Rs.s_root fullWidthAttr
                    |> Rs.s_header fullWidthAttr
                    |> Rs.s_closeIcon [ [ Css.cursor Css.pointer ] |> css, onClickWithStop closeMsg ]
                )
                { root = { headerTitle = Locale.string vc.locale "Tags list" }
                , identifierWithCopyIcon =
                    { chevronInstance = none
                    , copyIconInstance = Id.id id |> copyIconPathfinder vc
                    , identifier = Id.id id
                    , addTagIconInstance = none
                    }
                }
    in
    div
        [ css
            (TagsComponents.dialogTagsListComponent_details.styles ++ [ Css.width (Css.pct 100) ])
        ]
        [ header
        , View.Graph.Table.table
            TagsTable.styles
            vc
            [ css
                [ Css.verticalAlign Css.top
                , Css.overflowY Css.scroll
                , Css.overflowX Css.hidden
                ]
            ]
            View.Graph.Table.noTools
            (TagsTable.config vc)
            tags
        ]
