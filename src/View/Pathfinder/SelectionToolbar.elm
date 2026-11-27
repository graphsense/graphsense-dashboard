module View.Pathfinder.SelectionToolbar exposing (view)

{-| A small toolbar that floats above a multi-selection on the graph, so the
actions for several nodes are visible without knowing the right-click menu
(a "quick actions" bar above the selection; light, not dark).

Draft: the actions that exist for a multi-selection today. The statistics
stay in the side panel (`View.Pathfinder.SelectionStats`).

-}

import Animation
import Config.View as View
import Css
import Dict
import Html.Styled exposing (Html, div, span, text)
import Html.Styled.Attributes exposing (css, title)
import Model.Graph.Coords exposing (BBox)
import Model.Graph.Transform as Transform
import Model.Pathfinder as Pathfinder exposing (unit)
import Model.Pathfinder.ContextMenu exposing (ContextMenuType(..))
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..))
import Msg.Pathfinder exposing (Msg(..))
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Util.View exposing (onClickWithStop, testId)
import View.Locale as Locale


{-| Height of the toolbar plus the gap to the selection, in screen pixels.
-}
offsetAbove : Float
offsetAbove =
    56


view : View.Config -> Pathfinder.Model -> List MultiSelectOptions -> BBox -> Html Msg
view vc model selection dim =
    let
        points =
            selection
                |> List.filterMap
                    (\o ->
                        case o of
                            MSelectedAddress id ->
                                Dict.get id model.network.addresses
                                    |> Maybe.map (\a -> { x = (a.x + a.dx) * unit, y = (Animation.getTo a.y + a.dy) * unit, id = id })

                            MSelectedTx id ->
                                Dict.get id model.network.txs
                                    |> Maybe.map (\t -> { x = (t.x + t.dx) * unit, y = (Animation.getTo t.y + t.dy) * unit, id = id })
                    )

        current =
            Transform.getCurrent model.transform

        viewBox =
            Transform.coordsToBBox dim current

        z =
            Transform.getZ model.transform

        toScreen p =
            { x = (p.x - viewBox.x) / z, y = (p.y - viewBox.y) / z }
    in
    case ( List.map .x points, List.map .y points, List.head points ) of
        ( x :: xs, y :: ys, Just first ) ->
            let
                -- above the topmost node, centred over the selection; kept
                -- on screen
                anchor =
                    toScreen
                        { x = (List.foldl min x xs + List.foldl max x xs) / 2
                        , y = List.foldl min y ys - unit
                        }

                left =
                    clamp 12 (dim.width - 12) anchor.x

                top =
                    max 64 (anchor.y - offsetAbove)

                button tid label icon msg =
                    span
                        [ testId tid
                        , title label
                        , onClickWithStop msg
                        , css
                            [ Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            , Css.width (Css.px 28)
                            , Css.height (Css.px 28)
                            , Css.borderRadius (Css.px 4)
                            , Css.cursor Css.pointer
                            , Css.hover [ Css.property "background-color" Colors.greyBlue50 ]
                            ]
                        ]
                        [ icon ]

                separator =
                    span [ css [ Css.width (Css.px 1), Css.height (Css.px 18), Css.property "background-color" Colors.greyBlue100, Css.margin2 Css.zero (Css.px 4) ] ] []
            in
            div
                [ testId "gs-selection-toolbar"
                , css
                    [ Css.position Css.absolute
                    , Css.left (Css.px left)
                    , Css.top (Css.px top)
                    , Css.transform (Css.translateX (Css.pct -50))
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.property "gap" "2px"
                    , Css.padding2 (Css.px 4) (Css.px 6)
                    , Css.borderRadius (Css.px 8)
                    , Css.property "background-color" Colors.white
                    , Css.property "border" ("1px solid " ++ Colors.greyBlue100)
                    , Css.property "box-shadow" ("0 4px 12px " ++ Colors.greyShadow)
                    , Css.property "pointer-events" "all"
                    , Css.whiteSpace Css.noWrap
                    ]
                ]
                [ span
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.property "gap" "4px"
                        , Css.height (Css.px 26)
                        , Css.padding4 Css.zero (Css.px 4) Css.zero (Css.px 10)
                        , Css.borderRadius (Css.px 13)
                        , Css.property "background-color" Colors.blue50
                        , Css.property "color" Colors.blue700
                        , Css.fontSize (Css.px 12)
                        , Css.fontWeight (Css.int 600)
                        ]
                    ]
                    [ text (Locale.interpolated vc.locale "{0} selected" [ String.fromInt (List.length selection) ])
                    , span
                        [ testId "gs-selection-toolbar-clear"
                        , title (Locale.string vc.locale "Clear selection")
                        , onClickWithStop UserClosedDetailsView
                        , css
                            [ Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            , Css.width (Css.px 18)
                            , Css.height (Css.px 18)
                            , Css.borderRadius (Css.px 9)
                            , Css.cursor Css.pointer
                            , Css.hover [ Css.property "background-color" Colors.blue100 ]
                            ]
                        ]
                        [ Icons.iconsCloseSnoPadding {} ]
                    ]
                , separator
                , button "gs-selection-toolbar-align" (Locale.string vc.locale "Align horizontally") (Icons.iconsHorizontalAlign {}) UserClickedContextMenuAlignHorizontally
                , button "gs-selection-toolbar-annotate" (Locale.string vc.locale "Annotate") (Icons.iconsAnnotateS {}) UserToggleAnnotationSettings
                , button "gs-selection-toolbar-newtab" (Locale.string vc.locale "Open in new tab") (Icons.iconsGoToS {}) (UserClickedContextMenuOpenInNewTab (AddressContextMenu first.id))
                , separator
                , button "gs-selection-toolbar-delete" (Locale.string vc.locale "Remove from graph") (Icons.iconsDeleteS {}) UserClickedToolbarDeleteIcon
                ]

        _ ->
            text ""
