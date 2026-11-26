module View.Pathfinder.GraphFilter exposing (popover, statusRow, toolbarButton)

{-| The on-graph filter UI: the toolbar button, its popover and the row of
active-filter chips above the graph.

Built from existing components (switch, buttons, icons) and the theme colours.
The chips, the count badge and the popover frame are drawn here until the
Figma components of the "Graph filters" frame are approved and generated;
then they move to the generated `Theme` modules.

-}

import Config.View as View
import Css
import Dict
import Hovercard
import Html.Styled as Html exposing (Html, div, input, span, text)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Events exposing (onInput)
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.GraphFilter as GraphFilter
import Model.Pathfinder.Tools exposing (ToolbarHovercardType(..), toolbarHovercardTypeToId)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.GraphFilter as GraphFilterMsg
import RecordSetter as Rs
import Set
import Theme.Colors as Colors
import Theme.Html.Buttons as Buttons
import Theme.Html.Icons as Icons
import Theme.Html.SelectionControls exposing (SwitchSize(..))
import Util.Css
import Util.View exposing (hovercard, onClickWithStop, testId)
import View.Button as Button
import View.Controls as Controls
import View.Locale as Locale


msg : GraphFilterMsg.Msg -> Msg
msg =
    GraphFilterMsg



-- TOOLBAR


{-| The filter icon for the toolbar, with the number of active filters.
Highlighted like the other toolbar tools while the popover is open.
-}
toolbarButton : View.Config -> { activeFilters : Int, open : Bool } -> Html Msg
toolbarButton vc { activeFilters, open } =
    div
        [ HA.id (toolbarHovercardTypeToId Filter)
        , testId "gs-toolbar-filter"
        , HA.title (Locale.string vc.locale "Filters")
        , onClickWithStop (msg GraphFilterMsg.UserClickedToggleFilter)
        , css
            [ Css.position Css.relative
            , Css.width (Css.px 24)
            , Css.height (Css.px 24)
            , Css.cursor Css.pointer
            , Css.borderRadius (Css.px 5)
            , Css.property "background-color" <|
                if open then
                    Colors.toolbarHighlight

                else
                    "transparent"
            ]
        ]
        (Icons.iconsFilter {}
            :: (if activeFilters > 0 then
                    [ badge activeFilters ]

                else
                    []
               )
        )


badge : Int -> Html msg
badge n =
    span
        [ testId "gs-filter-count"
        , css
            [ Css.position Css.absolute
            , Css.top (Css.px -6)
            , Css.right (Css.px -7)
            , Css.minWidth (Css.px 16)
            , Css.height (Css.px 16)
            , Css.padding2 Css.zero (Css.px 4)
            , Css.borderRadius (Css.px 8)
            , Css.property "background-color" Colors.blue500
            , Css.property "color" Colors.white
            , Css.fontSize (Css.px 10)
            , Css.fontWeight (Css.int 600)
            , Css.lineHeight (Css.px 16)
            , Css.textAlign Css.center
            , Css.boxSizing Css.borderBox
            ]
        ]
        [ text (String.fromInt n) ]



-- POPOVER


popover : View.Config -> Pathfinder.Model -> Hovercard.Model -> Html Msg
popover vc model hc =
    let
        f =
            model.graphFilter

        fiat =
            Locale.fiat vc.locale vc.preferredFiatCurrency

        below =
            GraphFilter.countBelowLimit vc.preferredFiatCurrency model.config.tracingMode f.limit model.network

        assets =
            GraphFilter.assetsOnGraph model.config.tracingMode model.network
    in
    div
        [ testId "gs-graph-filter"
        , css
            [ Css.width (Css.px 320)
            , Css.property "color" Colors.greyBlue900
            , Css.fontSize (Css.px 12)
            ]
        ]
        [ div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.padding3 (Css.px 12) (Css.px 12) (Css.px 4) ] ]
            [ span [ css [ Css.flexGrow (Css.num 1), Css.fontSize (Css.px 14), Css.fontWeight (Css.int 600) ] ]
                [ Locale.text vc.locale "Filters" ]
            , span
                [ css [ Css.cursor Css.pointer, Css.displayFlex ]
                , HA.title (Locale.string vc.locale "Close")
                , onClickWithStop (msg GraphFilterMsg.UserClickedCloseFilter)
                ]
                [ Icons.iconsCloseSnoPadding {} ]
            ]
        , div [ css [ Css.padding3 (Css.px 8) (Css.px 12) (Css.px 12), Css.displayFlex, Css.flexDirection Css.column, Css.property "gap" "18px" ] ]
            [ div []
                [ div [ css [ Css.displayFlex, Css.alignItems Css.flexStart, Css.property "gap" "10px" ] ]
                    [ div [ css [ Css.flexGrow (Css.num 1) ] ]
                        [ div [ css [ Css.fontWeight (Css.int 600) ] ] [ Locale.text vc.locale "Hide tiny transfers" ]
                        , div
                            [ css
                                [ Css.property "color" <|
                                    if f.hideTiny then
                                        Colors.blue500

                                    else
                                        Colors.greyBlue500
                                , Css.marginTop (Css.px 2)
                                ]
                            ]
                            [ text <|
                                if f.hideTiny then
                                    Locale.interpolated vc.locale "{0} transfers under {1} hidden" [ String.fromInt below, fiat f.limit ]

                                else
                                    Locale.interpolated vc.locale "Dust and gas top-ups, under {0}" [ fiat f.limit ]
                            ]
                        ]
                    , span [ testId "gs-filter-tiny" ]
                        [ Controls.toggle
                            { size = SwitchSizeBig
                            , selected = f.hideTiny
                            , disabled = False
                            , msg = msg GraphFilterMsg.UserToggledHideTiny
                            }
                        ]
                    ]
                , if f.editingLimit then
                    limitEditor vc model

                  else
                    div [ css [ Css.displayFlex, Css.marginTop (Css.px 4) ] ]
                        [ link vc (Locale.string vc.locale "Change limit") (msg GraphFilterMsg.UserClickedChangeLimit) ]
                ]
            , div []
                [ div [ css [ Css.fontWeight (Css.int 600), Css.marginBottom (Css.px 8) ] ] [ Locale.text vc.locale "Assets" ]
                , if List.isEmpty assets then
                    div [ css [ Css.property "color" Colors.greyBlue500 ] ] [ Locale.text vc.locale "No transfers on the graph yet" ]

                  else
                    div [ css [ Css.displayFlex, Css.flexWrap Css.wrap, Css.alignItems Css.center, Css.property "gap" "4px" ] ]
                        (List.map
                            (\asset ->
                                optionChip asset
                                    (not (Set.member asset f.excludedAssets))
                                    (msg (GraphFilterMsg.UserToggledAsset asset))
                            )
                            assets
                            ++ (if Dict.isEmpty model.network.conversions then
                                    []

                                else
                                    [ span [ css [ Css.property "color" Colors.greyBlue500, Css.fontSize (Css.px 11), Css.marginLeft (Css.px 4) ] ]
                                        [ Locale.text vc.locale "swaps stay linked" ]
                                    ]
                               )
                        )
                ]
            ]
        , div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.property "gap" "8px"
                , Css.padding2 (Css.px 10) (Css.px 12)
                , Css.borderTop3 (Css.px 1) Css.solid Css.transparent
                , Css.property "border-top-color" Colors.greyBlue50
                ]
            ]
            [ Controls.toggle
                { size = SwitchSizeBig
                , selected = f.showHiddenGrey
                , disabled = False
                , msg = msg GraphFilterMsg.UserToggledShowHidden
                }
            , span [ css [ Css.flexGrow (Css.num 1), Css.property "color" Colors.greyBlue700 ] ]
                [ Locale.text vc.locale "Show hidden edges in grey" ]
            , link vc (Locale.string vc.locale "Reset") (msg GraphFilterMsg.UserClickedReset)
            ]
        ]
        |> Html.toUnstyled
        |> List.singleton
        |> hovercard vc hc (Util.Css.zIndexMainValue + 1)


limitEditor : View.Config -> Pathfinder.Model -> Html Msg
limitEditor vc model =
    div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "gap" "6px"
            , Css.marginTop (Css.px 8)
            , Css.padding2 (Css.px 8) (Css.px 10)
            , Css.borderRadius (Css.px 6)
            , Css.property "background-color" Colors.greyBlue20
            ]
        ]
        [ span [ css [ Css.property "color" Colors.greyBlue700 ] ] [ Locale.text vc.locale "Hide transfers under" ]
        , input
            [ HA.type_ "number"
            , HA.min "0"
            , HA.step "any"
            , testId "gs-filter-limit"
            , HA.value (String.fromFloat model.graphFilter.limit)
            , onInput
                (String.toFloat
                    >> Maybe.withDefault model.graphFilter.limit
                    >> GraphFilterMsg.UserChangedLimit
                    >> msg
                )
            , css
                [ Css.width (Css.px 72)
                , Css.height (Css.px 28)
                , Css.padding2 Css.zero (Css.px 8)
                , Css.borderRadius (Css.px 4)
                , Css.border3 (Css.px 1) Css.solid Css.transparent
                , Css.property "border-color" Colors.greyBlue200
                , Css.boxSizing Css.borderBox
                ]
            ]
            []
        , span [ css [ Css.property "color" Colors.greyBlue700 ] ] [ text (String.toUpper vc.preferredFiatCurrency) ]
        , link vc (Locale.string vc.locale "Done") (msg GraphFilterMsg.UserClickedLimitDone)
        ]


optionChip : String -> Bool -> Msg -> Html Msg
optionChip label selected onClick =
    span
        [ testId "gs-filter-asset"
        , HA.attribute "aria-pressed"
            (if selected then
                "true"

             else
                "false"
            )
        , onClickWithStop onClick
        , css
            [ Css.cursor Css.pointer
            , Css.height (Css.px 24)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.padding2 Css.zero (Css.px 10)
            , Css.borderRadius (Css.px 12)
            , Css.border3 (Css.px 1) Css.solid Css.transparent
            , Css.boxSizing Css.borderBox
            , Css.property "border-color" <|
                if selected then
                    Colors.blue500

                else
                    Colors.greyBlue200
            , Css.property "background-color" <|
                if selected then
                    Colors.blue50

                else
                    Colors.white
            , Css.property "color" <|
                if selected then
                    Colors.blue700

                else
                    Colors.greyBlue700
            , Css.fontWeight <|
                if selected then
                    Css.int 500

                else
                    Css.int 400
            ]
        ]
        [ text (String.toUpper label) ]


{-| `label` comes translated, so check-lang sees the literal key at the call.
-}
link : View.Config -> String -> Msg -> Html Msg
link vc label onClick =
    Button.defaultConfig
        |> Rs.s_text label
        |> Rs.s_onClick (Just onClick)
        |> Rs.s_onClickWithStop True
        |> Rs.s_size Buttons.ButtonSizeSmall
        |> Button.linkButtonBlue vc



-- STATUS ROW


{-| The chips above the graph while a filter is on, so nobody forgets the graph
is filtered. Nothing while no filter is on.
-}
statusRow : View.Config -> Pathfinder.Model -> Html Msg
statusRow vc model =
    let
        f =
            model.graphFilter

        r =
            model.highlighting.filter

        hidden =
            Set.size r.hiddenTxs + Set.size r.hiddenAggEdges

        included =
            GraphFilter.assetsOnGraph model.config.tracingMode model.network
                |> List.filter (\a -> not (Set.member a f.excludedAssets))
                |> List.map String.toUpper

        chips =
            List.filterMap identity
                [ if f.hideTiny then
                    Just
                        ( Locale.string vc.locale "Hiding"
                        , Locale.interpolated vc.locale "tiny transfers (< {0})" [ Locale.fiat vc.locale vc.preferredFiatCurrency f.limit ]
                        , GraphFilterMsg.UserToggledHideTiny
                        )

                  else
                    Nothing
                , f.period
                    |> Maybe.map
                        (\{ from, until } ->
                            ( Locale.string vc.locale "Time"
                            , Locale.timestampDateUniform vc.locale from ++ " – " ++ Locale.timestampDateUniform vc.locale until
                            , GraphFilterMsg.UserChangedPeriod Nothing
                            )
                        )
                , if Set.isEmpty f.excludedAssets then
                    Nothing

                  else
                    Just
                        ( Locale.string vc.locale "Assets"
                        , if List.isEmpty included then
                            Locale.string vc.locale "none"

                          else
                            String.join ", " included
                        , GraphFilterMsg.UserClickedClearAssets
                        )
                ]
    in
    if not (GraphFilter.isActive f) then
        Html.text ""

    else
        div
            [ testId "gs-filter-status"
            , css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                , Css.alignItems Css.center
                , Css.property "gap" "6px"
                , Css.property "flex-basis" "100%"
                , Css.property "pointer-events" "all"
                , Css.fontSize (Css.px 12)
                ]
            ]
            (summaryChip vc f.showHiddenGrey hidden
                :: List.map (filterChip vc) chips
                ++ [ link vc (Locale.string vc.locale "Clear all") (msg GraphFilterMsg.UserClickedReset) ]
            )


summaryChip : View.Config -> Bool -> Int -> Html Msg
summaryChip vc grey hidden =
    span
        (testId "gs-filter-summary" :: chipStyle Colors.white)
        [ text <|
            case ( grey, hidden ) of
                ( True, 1 ) ->
                    Locale.string vc.locale "Filters on · 1 edge greyed out"

                ( True, _ ) ->
                    Locale.interpolated vc.locale "Filters on · {0} edges greyed out" [ String.fromInt hidden ]

                ( False, 1 ) ->
                    Locale.string vc.locale "Filters on · 1 edge hidden"

                ( False, _ ) ->
                    Locale.interpolated vc.locale "Filters on · {0} edges hidden" [ String.fromInt hidden ]
        ]


filterChip : View.Config -> ( String, String, GraphFilterMsg.Msg ) -> Html Msg
filterChip vc ( label, value, clear ) =
    span
        (testId "gs-filter-chip" :: chipStyle Colors.greyBlue50)
        [ span [ css [ Css.property "color" Colors.greyBlue500 ] ] [ text (label ++ ":") ]
        , span [ css [ Css.property "color" Colors.greyBlue900 ] ] [ text value ]
        , span
            [ css [ Css.cursor Css.pointer, Css.displayFlex, Css.marginLeft (Css.px 2) ]
            , HA.title (Locale.string vc.locale "Remove this filter")
            , testId "gs-filter-chip-remove"
            , onClickWithStop (msg clear)
            ]
            [ Icons.iconsCloseSnoPadding {} ]
        ]


chipStyle : String -> List (Html.Attribute msg)
chipStyle background =
    [ css
        [ Css.displayFlex
        , Css.alignItems Css.center
        , Css.property "gap" "4px"
        , Css.height (Css.px 24)
        , Css.padding2 Css.zero (Css.px 10)
        , Css.borderRadius (Css.px 12)
        , Css.border3 (Css.px 1) Css.solid Css.transparent
        , Css.property "border-color" Colors.greyBlue100
        , Css.property "background-color" background
        , Css.property "color" Colors.greyBlue700
        , Css.boxSizing Css.borderBox
        , Css.whiteSpace Css.noWrap
        ]
    ]
