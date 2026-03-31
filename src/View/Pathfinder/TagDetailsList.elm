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
import Theme.Html.Icons as Icons
import Theme.Html.TagsComponents as TagsComponents
import Util.View exposing (copyIconPathfinder, none, onClickWithStop)
import View.Graph.Table
import View.Locale as Locale
import View.Pathfinder.Table.TagsTable as TagsTable


view : View.Config -> Msg -> Id -> Table Api.Data.AddressTag -> Bool -> Html Msg
view vc closeMsg id tags isClusterTagsList =
    let
        fullWidthAttr =
            Css.pct 100 |> Css.width |> List.singleton |> css |> List.singleton

        headerInstances =
            if isClusterTagsList then
                TagsComponents.dialogTagHeaderInstances
                    |> Rs.s_icon (Just (Icons.iconsTagLTypeIndirect {}))

            else
                TagsComponents.dialogTagHeaderInstances

        header =
            TagsComponents.dialogTagHeaderWithInstances
                (TagsComponents.dialogTagHeaderAttributes
                    |> Rs.s_root fullWidthAttr
                    |> Rs.s_header fullWidthAttr
                    |> Rs.s_icon
                        [ css
                            [ Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            ]
                        ]
                    |> Rs.s_closeIcon [ [ Css.cursor Css.pointer ] |> css, onClickWithStop closeMsg ]
                )
                headerInstances
                { root =
                    { headerTitle =
                        if isClusterTagsList then
                            Locale.string vc.locale "cluster tags list"

                        else
                            Locale.string vc.locale "tags list"
                    }
                , identifierWithCopyIcon =
                    { chevronInstance = none
                    , copyIconInstance = Id.id id |> copyIconPathfinder vc
                    , identifier = Id.id id
                    , addTagIconInstance = none
                    }
                }

        disclaimer =
            div
                [ css
                    [ Css.width (Css.pct 100)
                    , Css.margin2 (Css.px 16) (Css.px 0)
                    , Css.color (Css.hex "cc0000")
                    ]
                ]
                [ Html.Styled.text (Locale.string vc.locale "cluster tags disclaimer") ]
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
        , if isClusterTagsList then
            disclaimer

          else
            Html.Styled.text ""
        ]
