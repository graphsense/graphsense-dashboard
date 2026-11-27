module View.Pathfinder.GraphFilter exposing (popover, statusRow, toolbarButton)

{-| The on-graph filter UI: the toolbar button, its popover and the row of
active-filter chips above the graph.

Built from existing components (switch, buttons, icons) and the theme colours.
The chips, the count badge and the popover frame are drawn here until the
Figma components of the "Graph filters" frame are approved and generated;
then they move to the generated `Theme` modules.

-}

import Config.Pathfinder exposing (TracingMode(..))
import Config.View as View
import Css
import Dict
import Hovercard
import Html.Styled as Html exposing (Html, div, input, span, text)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Events exposing (onInput, stopPropagationOn)
import Json.Decode
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.GraphFilter as GraphFilter exposing (Bucket, Period, TimeMode(..))
import Model.Pathfinder.Tools exposing (ToolbarHovercardType(..), toolbarHovercardTypeToId)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.GraphFilter as GraphFilterMsg
import RecordSetter as Rs
import Set
import Theme.Colors as Colors
import Theme.Html.Buttons as Buttons
import Theme.Html.GraphFilters as GraphFilters
import Theme.Html.Icons as Icons
import Theme.Html.SelectionControls exposing (SwitchSize(..))
import Time
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
    GraphFilters.toolbarFilterButtonWithAttributes
        (GraphFilters.toolbarFilterButtonAttributes
            |> Rs.s_root
                [ HA.id (toolbarHovercardTypeToId Filter)
                , testId "gs-toolbar-filter"
                , HA.title (Locale.string vc.locale "Filters")
                , onClickWithStop (msg GraphFilterMsg.UserClickedToggleFilter)
                , css
                    [ Css.cursor Css.pointer
                    , Css.borderRadius (Css.px 5)
                    , Css.property "background-color"
                        (if open then
                            Colors.toolbarHighlight

                         else
                            "transparent"
                        )
                        |> Css.important
                    ]
                ]
            |> Rs.s_countBadge [ testId "gs-filter-count" ]
        )
        { countBadge = { count = String.fromInt activeFilters }
        , root = { badgeVisible = activeFilters > 0 }
        }



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
            -- the hovercard draws only the tick; the panel brings its own
            -- background, as the display settings panel does
            [ Css.width (Css.px 320)
            , Css.property "background-color" Colors.white
            , Css.borderRadius (Css.px 5)
            , Css.border3 (Css.px 1) Css.solid Css.transparent
            , Css.property "border-color" Colors.greyBlue100
            , Css.property "box-shadow" ("0 4px 12px " ++ Colors.greyShadow)
            , Css.overflow Css.hidden
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
            , timeSection vc model
            , div []
                [ div [ css [ Css.fontWeight (Css.int 600), Css.marginBottom (Css.px 8) ] ] [ Locale.text vc.locale "Assets" ]
                , if List.isEmpty assets then
                    div [ css [ Css.property "color" Colors.greyBlue500 ] ] [ Locale.text vc.locale "No transfers on the graph yet" ]

                  else
                    div []
                        [ div [ css [ Css.displayFlex, Css.flexWrap Css.wrap, Css.alignItems Css.center, Css.property "gap" "4px" ] ]
                            (List.map
                                (\asset ->
                                    optionChip (String.toUpper asset)
                                        (not (Set.member asset f.excludedAssets))
                                        (msg (GraphFilterMsg.UserToggledAsset asset))
                                )
                                assets
                            )
                        , if Dict.isEmpty model.network.conversions then
                            text ""

                          else
                            div [ css [ Css.property "color" Colors.greyBlue500, Css.fontSize (Css.px 11), Css.marginTop (Css.px 6) ] ]
                                [ Locale.text vc.locale "A swap stays on the graph as long as one of its two assets is shown." ]
                        ]
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
            [ span [ testId "gs-filter-grey" ]
                [ Controls.toggle
                    { size = SwitchSizeBig
                    , selected = f.showHiddenGrey
                    , disabled = False
                    , msg = msg GraphFilterMsg.UserToggledShowHidden
                    }
                ]
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
    GraphFilters.limitEditorWithInstances
        (GraphFilters.limitEditorAttributes
            |> Rs.s_root
                [ css
                    -- as wide as the popover, which is narrower than the frame
                    [ Css.marginTop (Css.px 8)
                    , Css.width (Css.pct 100) |> Css.important
                    , Css.boxSizing Css.borderBox |> Css.important
                    ]
                ]
        )
        (GraphFilters.limitEditorInstances |> Rs.s_limitInput (Just (limitInput model)))
        { root =
            { label = Locale.string vc.locale "Hide transfers under"
            , currency = String.toUpper vc.preferredFiatCurrency
            }
        , done = { variant = link vc (Locale.string vc.locale "Done") (msg GraphFilterMsg.UserClickedLimitDone) }
        }


{-| The field of the limit editor: a number input in place of the drawn one.
The arrows move the limit by 10; any value can still be typed.
-}
limitInput : Pathfinder.Model -> Html Msg
limitInput model =
    input
        [ HA.type_ "number"
        , HA.min "0"
        , HA.step "10"
        , testId "gs-filter-limit"
        , HA.value (String.fromFloat model.graphFilter.limit)
        , onInput
            (String.toFloat
                >> Maybe.withDefault model.graphFilter.limit
                >> GraphFilterMsg.UserChangedLimit
                >> msg
            )
        , css
            [ Css.width (Css.px 140)
            , Css.height (Css.px 32)
            , Css.padding2 Css.zero (Css.px 8)
            , Css.fontSize (Css.px 13)
            , Css.borderRadius (Css.px 4)
            , Css.border3 (Css.px 1) Css.solid Css.transparent
            , Css.property "border-color" Colors.greyBlue200
            , Css.boxSizing Css.borderBox
            ]
        ]
        []


optionChip : String -> Bool -> Msg -> Html Msg
optionChip label selected onClick =
    GraphFilters.optionChipWithAttributes
        { option = []
        , root =
            [ testId "gs-filter-asset"
            , HA.attribute "aria-pressed"
                (if selected then
                    "true"

                 else
                    "false"
                )
            , onClickWithStop onClick
            , css [ Css.cursor Css.pointer ]
            ]
        }
        { root =
            { selected =
                if selected then
                    GraphFilters.OptionChipSelectedTrue

                else
                    GraphFilters.OptionChipSelectedFalse
            , option = label
            }
        }


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
                            , periodLabel vc f.timeMode { from = from, until = until }
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
    GraphFilters.filterSummaryChipWithAttributes
        (GraphFilters.filterSummaryChipAttributes |> Rs.s_root [ testId "gs-filter-summary" ])
        { root =
            { summary =
                case ( grey, hidden ) of
                    ( True, 1 ) ->
                        Locale.string vc.locale "Filters on · 1 edge greyed out"

                    ( True, _ ) ->
                        Locale.interpolated vc.locale "Filters on · {0} edges greyed out" [ String.fromInt hidden ]

                    ( False, 1 ) ->
                        Locale.string vc.locale "Filters on · 1 edge hidden"

                    ( False, _ ) ->
                        Locale.interpolated vc.locale "Filters on · {0} edges hidden" [ String.fromInt hidden ]
            }
        }


filterChip : View.Config -> ( String, String, GraphFilterMsg.Msg ) -> Html Msg
filterChip vc ( label, value, clear ) =
    GraphFilters.filterChipWithAttributes
        (GraphFilters.filterChipAttributes
            |> Rs.s_root [ testId "gs-filter-chip" ]
            |> Rs.s_remove
                [ css [ Css.cursor Css.pointer ]
                , HA.title (Locale.string vc.locale "Remove this filter")
                , testId "gs-filter-chip-remove"
                , onClickWithStop (msg clear)
                ]
        )
        { root = { label = label ++ ":", value = value } }



-- TIME


{-| The most bars the activity strip draws; longer spans get several days per
bar.
-}
maxBars : Int
maxBars =
    60


timeSection : View.Config -> Pathfinder.Model -> Html Msg
timeSection vc model =
    let
        f =
            model.graphFilter

        zone =
            vc.locale.zone

        buckets =
            GraphFilter.activity vc.preferredFiatCurrency zone maxBars model.config.tracingMode f model.network

        modeChip label mode =
            optionChip label (f.timeMode == mode) (msg (GraphFilterMsg.UserChoseTimeMode mode))

        dateValue =
            Maybe.map (GraphFilter.toDateString zone) >> Maybe.withDefault ""

        bounds =
            case ( List.head buckets, List.reverse buckets |> List.head ) of
                ( Just first, Just last ) ->
                    [ HA.min (GraphFilter.toDateString zone first.period.from)
                    , HA.max (GraphFilter.toDateString zone last.period.until)
                    ]

                _ ->
                    []
    in
    div [ testId "gs-filter-time" ]
        [ div [ css [ Css.displayFlex, Css.alignItems Css.baseline, Css.marginBottom (Css.px 8) ] ]
            [ span [ css [ Css.flexGrow (Css.num 1), Css.fontWeight (Css.int 600) ] ] [ Locale.text vc.locale "Time" ]
            , f.period
                |> Maybe.map (\p -> span [ css [ Css.property "color" Colors.blue500 ] ] [ text (periodLabel vc f.timeMode p) ])
                |> Maybe.withDefault (text "")
            ]
        , case model.config.tracingMode of
            AggregateTracingMode ->
                div [ css [ Css.property "color" Colors.greyBlue500 ] ]
                    [ Locale.text vc.locale "The time filter works in the transaction view" ]

            TransactionTracingMode ->
                div [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.property "gap" "8px" ] ]
                    [ div [ css [ Css.displayFlex, Css.property "gap" "4px" ] ]
                        [ modeChip (Locale.string vc.locale "Any time") AnyTime
                        , modeChip (Locale.string vc.locale "One day") OneDay
                        , modeChip (Locale.string vc.locale "Date range") DateRange
                        ]

                    -- above the date fields: they appear when a drag starts,
                    -- and must not push the strip away from the pointer
                    , activityStrip vc f.period buckets
                    , case f.timeMode of
                        AnyTime ->
                            text ""

                        OneDay ->
                            dateField "gs-filter-day" bounds (dateValue (Maybe.map .from f.period)) GraphFilterMsg.UserChangedDay

                        DateRange ->
                            div [ css [ Css.displayFlex, Css.alignItems Css.center, Css.property "gap" "8px" ] ]
                                [ dateField "gs-filter-from" bounds (dateValue (Maybe.map .from f.period)) GraphFilterMsg.UserChangedFrom
                                , span [ css [ Css.property "color" Colors.greyBlue500 ] ] [ Locale.text vc.locale "to" ]
                                , dateField "gs-filter-until" bounds (dateValue (Maybe.map .until f.period)) GraphFilterMsg.UserChangedUntil
                                ]
                    ]
        ]


periodLabel : View.Config -> TimeMode -> Period -> String
periodLabel vc mode { from, until } =
    if mode == OneDay then
        Locale.timestampDateUniform vc.locale from

    else
        Locale.timestampDateUniform vc.locale from ++ " – " ++ Locale.timestampDateUniform vc.locale until


dateField : String -> List (Html.Attribute Msg) -> String -> (String -> GraphFilterMsg.Msg) -> Html Msg
dateField tid bounds value toMsg =
    input
        ([ HA.type_ "date"
         , testId tid
         , HA.value value
         , onInput (toMsg >> msg)
         , css
            [ Css.flexGrow (Css.num 1)
            , Css.minWidth Css.zero
            , Css.height (Css.px 30)
            , Css.padding2 Css.zero (Css.px 8)
            , Css.borderRadius (Css.px 4)
            , Css.border3 (Css.px 1) Css.solid Css.transparent
            , Css.property "border-color" Colors.greyBlue200
            , Css.property "color" Colors.greyBlue900
            , Css.boxSizing Css.borderBox
            , Css.fontSize (Css.px 12)
            ]
         ]
            ++ bounds
        )
        []


{-| Where the strip leaves out a long stretch without transfers (see
`GraphFilter.activity`): a white cut through the strip, labelled with how long
the stretch is; the dates are in its tooltip.
-}
gapMarker : View.Config -> Bucket -> List (Html Msg)
gapMarker vc bucket =
    case bucket.gapBefore of
        Just days ->
            let
                length =
                    Locale.durationToStringWithPrecision vc.locale 1 (days * dayMillis)

                until =
                    Time.posixToMillis bucket.period.from - 1

                from =
                    until + 1 - days * dayMillis
            in
            [ GraphFilters.activityStripGapWithAttributes
                (GraphFilters.activityStripGapAttributes
                    |> Rs.s_root
                        [ testId "gs-filter-strip-gap"
                        , HA.title
                            (Locale.interpolated vc.locale
                                "No transfers for {0} ({1} – {2}); left out of the strip"
                                [ length
                                , Locale.timestampDateUniform vc.locale (Time.millisToPosix from)
                                , Locale.timestampDateUniform vc.locale (Time.millisToPosix until)
                                ]
                            )
                        , css [ Css.property "flex" "0 0 auto", Css.margin2 Css.zero (Css.px 2) ]
                        ]
                )
                { root = { length = length } }
            ]

        Nothing ->
            []


dayMillis : Int
dayMillis =
    86400000


{-| Money moved per bar, height relative to the busiest bar (square root, so
small days stay visible next to a big one). Press a bar to pick it, drag to
pick a period. Bars outside the picked period are light.
-}
activityStrip : View.Config -> Maybe Period -> List Bucket -> Html Msg
activityStrip vc period buckets =
    let
        top =
            buckets |> List.map .value |> List.maximum |> Maybe.withDefault 0

        inPeriod b =
            case period of
                Just p ->
                    Time.posixToMillis b.period.from <= Time.posixToMillis p.until && Time.posixToMillis b.period.until >= Time.posixToMillis p.from

                Nothing ->
                    True

        mouse event toMsg b =
            stopPropagationOn event (Json.Decode.succeed ( msg (toMsg b.period), True ))

        bar b =
            div
                [ testId "gs-filter-strip-bar"
                , HA.title (Locale.timestampDateUniform vc.locale b.period.from ++ ": " ++ Locale.fiat vc.locale vc.preferredFiatCurrency b.value)
                , mouse "mousedown" GraphFilterMsg.UserPressedStripBucket b
                , mouse "mouseenter" GraphFilterMsg.UserEnteredStripBucket b
                , mouse "mouseup" GraphFilterMsg.UserReleasedStripBucket b
                , css
                    [ Css.flexGrow (Css.num 1)
                    , Css.flexBasis (Css.px 0)
                    , Css.height (Css.pct 100)
                    , Css.displayFlex
                    , Css.alignItems Css.flexEnd
                    , Css.justifyContent Css.center
                    , Css.cursor Css.pointer
                    , Css.property "background-color" <|
                        if period /= Nothing && inPeriod b then
                            Colors.blue50

                        else
                            "transparent"
                    ]
                ]
                [ if b.value > 0 && top > 0 then
                    div
                        [ css
                            [ Css.width (Css.pct 70)
                            , Css.maxWidth (Css.px 8)
                            , Css.height (Css.px (3 + sqrt (b.value / top) * 22))
                            , Css.marginBottom (Css.px 3)
                            , Css.borderRadius (Css.px 1)
                            , Css.property "background-color" <|
                                if inPeriod b then
                                    Colors.greyBlue500

                                else
                                    Colors.greyBlue100
                            ]
                        ]
                        []

                  else
                    text ""
                ]
    in
    case ( List.head buckets, List.reverse buckets |> List.head ) of
        ( Just first, Just last ) ->
            div []
                [ div
                    [ testId "gs-filter-strip"
                    , stopPropagationOn "mouseleave" (Json.Decode.succeed ( msg GraphFilterMsg.UserLeftStrip, True ))
                    , css
                        [ Css.displayFlex
                        , Css.height (Css.px 32)
                        , Css.padding2 Css.zero (Css.px 4)
                        , Css.borderRadius (Css.px 4)
                        , Css.property "background-color" Colors.greyBlue20
                        , Css.property "user-select" "none"
                        ]
                    ]
                    (List.concatMap (\b -> gapMarker vc b ++ [ bar b ]) buckets)
                , div [ css [ Css.displayFlex, Css.justifyContent Css.spaceBetween, Css.marginTop (Css.px 4), Css.fontSize (Css.px 11), Css.property "color" Colors.greyBlue500 ] ]
                    [ span [] [ text (Locale.timestampDateUniform vc.locale first.period.from) ]
                    , span [] [ Locale.text vc.locale "click a day, drag a period" ]
                    , span [] [ text (Locale.timestampDateUniform vc.locale last.period.from) ]
                    ]
                , if List.any (.gapBefore >> (/=) Nothing) buckets then
                    div [ css [ Css.marginTop (Css.px 2), Css.fontSize (Css.px 11), Css.property "color" Colors.greyBlue500 ] ]
                        [ Locale.text vc.locale "Long stretches without transfers are cut out of the strip; the white gaps say how long." ]

                  else
                    text ""
                ]

        _ ->
            text ""
