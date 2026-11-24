module View.Pathfinder.ActorTagsList exposing (view)

{-| The tags of an actor, across all its addresses (Binance has millions), as
a table that loads page by page while scrolling. Opened from "Show tags" in
the actor dialog. A click on an address adds it to the graph.

Same frame and header as the tags list of an address
(`View.Pathfinder.TagDetailsList`), with an address column in front.

-}

import Components.InfiniteTable as InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Model exposing (Msg(..))
import Model.Dialog as Dialog
import RecordSetter as Rs
import Theme.Html.TagsComponents as TagsComponents
import Util.View exposing (none, onClickWithStop, testId)
import Util.View.Loadingspinner as Loadingspinner
import View.Locale as Locale
import View.Pathfinder.Table.TagsTable as TagsTable


view : View.Config -> Dialog.ActorTagsConfig Msg -> Html Msg
view vc conf =
    let
        fullWidthAttr =
            Css.pct 100 |> Css.width |> List.singleton |> css |> List.singleton

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
                    |> Rs.s_closeIcon [ [ Css.cursor Css.pointer ] |> css, onClickWithStop conf.closeMsg ]
                )
                TagsComponents.dialogTagHeaderInstances
                { root =
                    { headerTitle = Locale.string vc.locale "tags list"
                    }
                , identifierWithCopyIcon =
                    { chevronInstance = none
                    , copyIconInstance = none
                    , identifier = conf.actorLabel
                    , addTagIconInstance = none
                    }
                }

        table =
            if InfiniteTable.isEmpty conf.table then
                if InfiniteTable.isLoading conf.table then
                    Loadingspinner.html []

                else
                    Html.Styled.text (Locale.string vc.locale "No tags found")

            else
                InfiniteTable.view (TagsTable.actorConfig vc ActorTagsDialogTableMsg UserClickedActorTagAddress)
                    [ css (Css.maxHeight (Css.px 500) :: fullWidth) ]
                    conf.table
    in
    div
        [ css
            [ Css.minWidth (Css.px 1000)
            , Css.minHeight (Css.px 600)
            ]
        , testId "gs-actor-tags-dialog"
        ]
        [ div
            [ css
                (TagsComponents.dialogTagsListComponent_details.styles ++ [ Css.width (Css.pct 100) ])
            ]
            [ header
            , table
            ]
        ]
