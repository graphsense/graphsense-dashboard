module Components.TransactionFilter exposing (Effect, FilterHeaderConfig, InternalModel, Model, Msg(..), QuickFilter, QuickFilterModel, Range, Settings, SettingsModel, applyQuickFilter, getDateRange, getDirection, getDirectionFromQuickFilter, getIncludeZeroValueTxs, getSelectedAsset, getSelectedQuickFilter, getSettings, getTx, getUtxoFilter, hasChanged, init, initQuickFilter, initSettings, initSettingsFromQuickFilter, perform, setFocusDate, setSelectedQuickFilter, subscriptions, update, updateDateRange, updateDateRangeInternal, updateDirection, updateQuickFilters, updateSelectedAsset, view, withAssetSelectBox, withDateRange, withDateRangePicker, withDirection, withIncludeZeroValueTxs, withQuickFilter)

import Basics.Extra exposing (flip)
import Browser.Events
import Components.ExportCSV as ExportCSV
import Components.Tooltip as Tooltip
import Config.DateRangePicker exposing (datePickerSettings)
import Config.View as View
import Css
import Css.DateTimePicker as DateTimePicker
import DurationDatePicker as DatePicker
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (on, onClick, stopPropagationOn)
import Init.DateRangePicker as DateRangePicker
import Json.Decode
import List.Extra
import Maybe.Extra
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import Model.Pathfinder.Tx as Tx
import RecordSetter as Rs exposing (s_direction, s_settings)
import String
import Svg.Styled.Attributes exposing (css)
import Theme.Colors
import Theme.Html.Icons as Icons
import Theme.Html.SelectionControls as Sc
import Theme.Html.SidePanelComponents as SidePanelComponents
import Time exposing (Posix)
import Update.DateRangePicker as DateRangePicker
import Util exposing (n)
import Util.Checkbox as Checkbox
import Util.Css
import Util.Data as Data
import Util.Pathfinder as Pathfinder
import Util.ThemedSelectBox as ThemedSelectBox
import Util.View exposing (fullWidthCss, none, pointer, truncateLongIdentifier)
import View.Button as Button
import View.Controls as Controls
import View.Locale as Locale


type Model
    = Internal InternalModel


type Effect
    = TooltipEffect Tooltip.Effect


type alias InternalModel =
    { dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , assetSelectBox : Maybe (ThemedSelectBox.Model (Maybe String))
    , quickFilterSelect : Maybe (ThemedSelectBox.Model (Maybe QuickFilterModel))
    , showCustomFilter : Bool
    , settings : SettingsModel
    , tooltip : Maybe Tooltip.Model
    , showDialog : Bool
    , dialogPosition : { top : Float, right : Float }
    , isDragging : Bool
    , dragStart : Maybe { x : Int, y : Int, top : Float, right : Float }
    }


type Settings
    = Settings SettingsModel


type alias SettingsModel =
    { range : Maybe (Maybe Range)
    , asset : Maybe String
    , direction : Maybe (Maybe Direction)
    , includeZeroValueTxs : Maybe Bool
    , utxoOnly : Bool
    }


type Range
    = Starting Posix
    | Until Posix
    | Range Posix Posix


type QuickFilter
    = QuickFilterInternal QuickFilterModel


type alias QuickFilterModel =
    { date : Posix
    , direction : Direction
    , tx : Tx.TxType
    }


getDateRange : Settings -> Maybe ( Maybe Posix, Maybe Posix )
getDateRange (Settings model) =
    model.range
        |> Maybe.map
            (\r ->
                case r of
                    Just (Starting start) ->
                        ( Just start, Nothing )

                    Just (Until end) ->
                        ( Nothing, Just end )

                    Just (Range start end) ->
                        ( Just start, Just end )

                    Nothing ->
                        ( Nothing, Nothing )
            )


getSelectedAsset : Settings -> Maybe String
getSelectedAsset (Settings model) =
    model.asset


getIncludeZeroValueTxs : Settings -> Maybe Bool
getIncludeZeroValueTxs (Settings model) =
    model.includeZeroValueTxs


type alias FilterHeaderConfig msg =
    { tag : Msg -> msg
    , exportCsv : Maybe ( ExportCSV.Msg -> msg, ExportCSV.Model )
    }


type Msg
    = ResetAllTxFilters
    | ResetDateRangePicker
    | ResetTxAssetFilter
    | ResetTxDirectionFilter
    | ResetZeroValueSubTxsTableFilters
    | TxTableFilterShowAllTxs
    | TxTableFilterShowIncomingTxOnly
    | TxTableFilterShowOutgoingTxOnly
    | TxTableFilterToggleZeroValue
    | TxTableAssetSelectBoxMsg (ThemedSelectBox.Msg (Maybe String))
    | OpenDateRangePicker
    | CloseDateRangePicker
    | UpdateDateRangePicker DatePicker.Msg
    | TxTableQuickFilterSelectBoxMsg (ThemedSelectBox.Msg (Maybe QuickFilterModel))
    | UserClickedCustomFilterLabel
    | UserClickedUtxoOnly
    | ResetTxUtxoOnlyFilter
    | ToggleDialog
    | StartDrag Int Int
    | Drag Int Int
    | EndDrag
    | TooltipMsg Tooltip.Msg


update : Msg -> Model -> ( Model, List Effect )
update msg (Internal model) =
    case msg of
        ResetAllTxFilters ->
            resetAll model
                |> Internal
                |> n

        ResetDateRangePicker ->
            resetDateRangePicker model
                |> Internal
                |> n

        ResetTxDirectionFilter ->
            resetDirection model
                |> Internal
                |> n

        ResetTxUtxoOnlyFilter ->
            resetUtxoOnly model
                |> Internal
                |> n

        ResetTxAssetFilter ->
            resetSelectedAsset model
                |> Internal
                |> n

        ResetZeroValueSubTxsTableFilters ->
            resetIncludeZeroValueTxs model
                |> Internal
                |> n

        OpenDateRangePicker ->
            { model | dateRangePicker = Maybe.map DateRangePicker.openPicker model.dateRangePicker }
                |> Internal
                |> n

        CloseDateRangePicker ->
            { model | dateRangePicker = Maybe.map DateRangePicker.closePicker model.dateRangePicker }
                |> Internal
                |> n

        UpdateDateRangePicker subMsg ->
            model.dateRangePicker
                |> Maybe.map
                    (\dateRangePicker ->
                        let
                            newPicker =
                                DateRangePicker.update subMsg dateRangePicker

                            changed =
                                newPicker.fromDate
                                    /= Nothing
                                    && newPicker.fromDate
                                    /= dateRangePicker.fromDate
                                    || newPicker.toDate
                                    /= Nothing
                                    && newPicker.toDate
                                    /= dateRangePicker.toDate
                        in
                        { model
                            | dateRangePicker =
                                newPicker
                                    |> (if changed then
                                            DateRangePicker.closePicker

                                        else
                                            identity
                                       )
                                    |> Just
                        }
                            |> updateDateRangeInternal ( newPicker.fromDate, newPicker.toDate )
                    )
                |> Maybe.withDefault model
                |> Internal
                |> n

        TxTableFilterShowAllTxs ->
            updateDirectionInternal Nothing model
                |> Internal
                |> n

        TxTableFilterShowIncomingTxOnly ->
            updateDirectionInternal (Just Incoming) model
                |> Internal
                |> n

        TxTableFilterShowOutgoingTxOnly ->
            updateDirectionInternal (Just Outgoing) model
                |> Internal
                |> n

        TxTableFilterToggleZeroValue ->
            model.settings.includeZeroValueTxs
                |> Maybe.map not
                |> flip Rs.s_includeZeroValueTxs model.settings
                |> flip s_settings model
                |> Internal
                |> n

        TxTableAssetSelectBoxMsg ms ->
            model.assetSelectBox
                |> Maybe.map
                    (\sb ->
                        let
                            ( newSelect, outMsg ) =
                                ThemedSelectBox.update ms sb
                        in
                        { model
                            | assetSelectBox = Just newSelect
                            , settings =
                                model.settings
                                    |> Rs.s_asset
                                        (case outMsg of
                                            ThemedSelectBox.Selected sel ->
                                                sel

                                            _ ->
                                                model.settings.asset
                                        )
                        }
                    )
                |> Maybe.withDefault model
                |> Internal
                |> n

        TxTableQuickFilterSelectBoxMsg ms ->
            model.quickFilterSelect
                |> Maybe.map
                    (\sb ->
                        let
                            ( newSelect, outMsg ) =
                                ThemedSelectBox.update ms sb
                        in
                        { model
                            | quickFilterSelect = Just newSelect
                        }
                            |> (case outMsg of
                                    ThemedSelectBox.Selected sel ->
                                        sel
                                            |> Maybe.map applyQuickFilter
                                            |> Maybe.withDefault resetAll

                                    _ ->
                                        identity
                               )
                    )
                |> Maybe.withDefault model
                |> Internal
                |> n

        UserClickedCustomFilterLabel ->
            { model | showCustomFilter = not model.showCustomFilter }
                |> Internal
                |> n

        UserClickedUtxoOnly ->
            settingsToQuickFilter model
                |> Maybe.map
                    (\_ ->
                        not model.settings.utxoOnly
                            |> flip Rs.s_utxoOnly model.settings
                            |> flip s_settings model
                    )
                |> Maybe.withDefault model
                |> Internal
                |> n

        ToggleDialog ->
            { model | showDialog = not model.showDialog }
                |> Internal
                |> n

        StartDrag x y ->
            { model
                | isDragging = True
                , dragStart = Just { x = x, y = y, top = model.dialogPosition.top, right = model.dialogPosition.right }
            }
                |> Internal
                |> n

        Drag x y ->
            case model.dragStart of
                Just start ->
                    let
                        dx =
                            toFloat (x - start.x)

                        dy =
                            toFloat (y - start.y)

                        newTop =
                            start.top + dy

                        newRight =
                            start.right - dx
                    in
                    { model
                        | dialogPosition = { top = newTop, right = newRight }
                        , isDragging = True
                    }
                        |> Internal
                        |> n

                Nothing ->
                    model
                        |> Internal
                        |> n

        EndDrag ->
            { model
                | isDragging = False
                , dragStart = Nothing
            }
                |> Internal
                |> n

        TooltipMsg tm ->
            model.tooltip
                |> Maybe.map
                    (\tt ->
                        let
                            ( tooltip, eff ) =
                                Tooltip.update tm tt
                        in
                        ( Internal
                            { model
                                | tooltip = Just tooltip
                            }
                        , List.map TooltipEffect eff
                        )
                    )
                |> Maybe.withDefault (model |> Internal |> n)


resetAll : InternalModel -> InternalModel
resetAll =
    resetSelectedAsset
        >> resetDateRangePicker
        >> resetDirection
        >> resetUtxoOnly
        >> resetIncludeZeroValueTxs


updateDirectionInternal : Maybe Direction -> InternalModel -> InternalModel
updateDirectionInternal direction model =
    model.settings.direction
        |> Maybe.map (\_ -> direction)
        |> flip s_direction model.settings
        |> flip s_settings model


updateDirection : Maybe Direction -> Model -> Model
updateDirection direction (Internal model) =
    updateDirectionInternal direction model
        |> Internal


resetIncludeZeroValueTxs : InternalModel -> InternalModel
resetIncludeZeroValueTxs model =
    model.settings.includeZeroValueTxs
        |> Maybe.map (\_ -> True)
        |> flip Rs.s_includeZeroValueTxs model.settings
        |> flip s_settings model


resetSelectedAsset : InternalModel -> InternalModel
resetSelectedAsset model =
    Rs.s_asset Nothing model.settings
        |> flip s_settings model


resetDateRangePicker : InternalModel -> InternalModel
resetDateRangePicker model =
    { model
        | dateRangePicker =
            model.dateRangePicker
                |> Maybe.map
                    (\drp ->
                        DateRangePicker.init UpdateDateRangePicker drp.focusDate Nothing Nothing drp.settings
                    )
        , settings =
            model.settings
                |> Rs.s_range
                    (model.settings.range
                        |> Maybe.map (\_ -> Nothing)
                    )
    }


resetDirection : InternalModel -> InternalModel
resetDirection model =
    model.settings.direction
        |> Maybe.map (\_ -> Nothing)
        |> flip s_direction model.settings
        |> flip s_settings model


resetUtxoOnly : InternalModel -> InternalModel
resetUtxoOnly model =
    model.settings
        |> Rs.s_utxoOnly False
        |> flip s_settings model


closeButtonGrey : msg -> Html msg
closeButtonGrey msg =
    Icons.iconsCloseBlackWithAttributes
        (Icons.iconsCloseBlackAttributes
            |> Rs.s_root
                [ [ Util.Css.overrideBlack Theme.Colors.greyBlue500 ] |> css
                , Util.View.pointer
                , onClick msg
                ]
        )
        {}


dateTimeFilterHeaderFromRange : View.Config -> Msg -> Maybe Range -> Html Msg
dateTimeFilterHeaderFromRange vc resetMsg maybeRange =
    let
        ( startDate, endDate ) =
            case maybeRange of
                Just (Starting start) ->
                    ( Just start, Nothing )

                Just (Until end) ->
                    ( Nothing, Just end )

                Just (Range start end) ->
                    ( Just start, Just end )

                Nothing ->
                    ( Nothing, Nothing )

        startDateStr =
            startDate
                |> Maybe.map (renderDate vc (Locale.isFirstSecondOfTheDay vc.locale))

        endDateStr =
            endDate
                |> Maybe.map (renderDate vc (Locale.isLastSecondOfTheDay vc.locale))
    in
    renderDateTimeFilter vc resetMsg startDateStr endDateStr


renderDateTimeFilter : View.Config -> Msg -> Maybe String -> Maybe String -> Html Msg
renderDateTimeFilter vc resetMsg startDate endDate =
    case ( startDate, endDate ) of
        ( Nothing, Nothing ) ->
            none

        ( Just startP, Just endP ) ->
            SidePanelComponents.filterLabel
                { root =
                    { iconInstance =
                        closeButtonGrey resetMsg
                    , text = endP
                    , separator = "-"
                    , start = startP
                    , showSeparator = True
                    , showStart = True
                    }
                }

        ( Just startP, Nothing ) ->
            SidePanelComponents.filterLabel
                { root =
                    { iconInstance =
                        closeButtonGrey resetMsg
                    , text = startP
                    , separator = Locale.string vc.locale "datefilter-starting"
                    , start = ""
                    , showSeparator = True
                    , showStart = False
                    }
                }

        ( Nothing, Just endP ) ->
            SidePanelComponents.filterLabel
                { root =
                    { iconInstance =
                        closeButtonGrey resetMsg
                    , text = endP
                    , separator = Locale.string vc.locale "datefilter-until"
                    , start = ""
                    , showSeparator = True
                    , showStart = False
                    }
                }


renderDate : View.Config -> (Posix -> Bool) -> Posix -> String
renderDate vc showTimeFn date =
    date
        |> (if showTimeFn date then
                Locale.timestampDateUniform vc.locale

            else
                Locale.timestampDateTimeUniform vc.locale False
           )


dateTimeFilterRawSmall : View.Config -> String -> String -> Html msg
dateTimeFilterRawSmall vc label text =
    SidePanelComponents.filterLabelSmall
        { root =
            { text = text
            , separator = Locale.string vc.locale label
            , start = ""
            , showSeparator = True
            , showStart = False
            }
        }


dateTimeFilterRaw : View.Config -> msg -> String -> String -> Html msg
dateTimeFilterRaw vc msg label text =
    SidePanelComponents.filterLabel
        { root =
            { iconInstance = closeButtonGrey msg
            , text = text
            , separator = Locale.string vc.locale label
            , start = ""
            , showSeparator = True
            , showStart = False
            }
        }


directionFilterHeader : View.Config -> msg -> Direction -> Html msg
directionFilterHeader vc resetMsg dir =
    directionFilterString dir
        |> stringFilterHeader vc
            resetMsg


directionFilterString : Direction -> String
directionFilterString dir =
    case dir of
        Incoming ->
            "incoming only"

        Outgoing ->
            "outgoing only"


stringFilterHeader : View.Config -> msg -> String -> Html msg
stringFilterHeader vc msg str =
    SidePanelComponents.filterLabel
        { root =
            { iconInstance =
                closeButtonGrey msg
            , start = ""
            , separator = ""
            , text =
                Locale.string vc.locale str
            , showSeparator = False
            , showStart = False
            }
        }


stringFilterSmall : View.Config -> String -> Html msg
stringFilterSmall vc str =
    SidePanelComponents.filterLabelSmall
        { root =
            { start = ""
            , separator = ""
            , text =
                Locale.string vc.locale str
            , showSeparator = False
            , showStart = False
            }
        }


assetFilterHeader : View.Config -> msg -> String -> Html msg
assetFilterHeader vc resetMsg =
    String.toUpper >> stringFilterHeader vc resetMsg


zeroValuesHeader : View.Config -> msg -> Bool -> Html msg
zeroValuesHeader vc resetMsg includeZeroValueTxs =
    if includeZeroValueTxs then
        none

    else
        stringFilterHeader vc resetMsg "filter-no-zero-value"


utxoOnlyHeader : View.Config -> msg -> Html msg
utxoOnlyHeader vc resetMsg =
    stringFilterHeader vc resetMsg "filter-utxo-only"


filterHeader : View.Config -> FilterHeaderConfig msg -> Model -> Html msg
filterHeader vc config (Internal model) =
    let
        qf =
            settingsToQuickFilter model

        utxoFilter =
            model
                |> Internal
                |> getUtxoFilter
                |> Maybe.map (\_ -> utxoOnlyHeader vc ResetTxUtxoOnlyFilter)

        asset =
            model.settings.asset |> Maybe.map (assetFilterHeader vc ResetTxAssetFilter)

        filterList =
            qf
                |> Maybe.map
                    (quickfilterHeader vc
                        >> Just
                        >> List.singleton
                        >> (::) utxoFilter
                        >> flip (++) [ asset ]
                    )
                |> Maybe.withDefault
                    [ model.settings.range |> Maybe.map (dateTimeFilterHeaderFromRange vc ResetDateRangePicker)
                    , model.settings.direction |> Maybe.Extra.join |> Maybe.map (directionFilterHeader vc ResetTxDirectionFilter)
                    , utxoFilter
                    , asset
                    , model.settings.includeZeroValueTxs |> Maybe.map (zeroValuesHeader vc ResetZeroValueSubTxsTableFilters)
                    ]
    in
    SidePanelComponents.sidePanelListFilterRowWithAttributes
        (SidePanelComponents.sidePanelListFilterRowAttributes
            |> Rs.s_root
                [ css [ fullWidthCss ]
                ]
            |> Rs.s_framedFilter
                [ onClick (ToggleDialog |> config.tag)
                , Util.View.pointer
                ]
            |> Rs.s_framedExport
                (config.exportCsv
                    |> Maybe.map
                        (\( tag, exportCSVModel ) ->
                            ExportCSV.attributes exportCSVModel
                                |> List.map (Attributes.map tag)
                        )
                    |> Maybe.withDefault
                        [ css [ Css.display Css.none ] ]
                )
            |> Rs.s_icons
                (let
                    dateFilterTakesMuchSpace =
                        (model.dateRangePicker
                            |> Maybe.andThen .fromDate
                            |> Maybe.map (Locale.isFirstSecondOfTheDay vc.locale >> not)
                            |> Maybe.withDefault False
                        )
                            && (model.dateRangePicker
                                    |> Maybe.andThen .toDate
                                    |> Maybe.map (Locale.isLastSecondOfTheDay vc.locale >> not)
                                    |> Maybe.withDefault False
                               )
                 in
                 if dateFilterTakesMuchSpace then
                    [ css [ Css.flexWrap Css.wrap, Css.width <| Css.px 32 ] ]

                 else
                    []
                )
        )
        { filterList =
            filterList
                |> List.filterMap identity
                |> List.map (Html.map config.tag)
        }
        { framedFilter =
            { iconInstance = Icons.iconsFilter {}
            }
        , framedExport =
            { iconInstance =
                config.exportCsv
                    |> Maybe.map
                        (\( _, exportCSVModel ) ->
                            ExportCSV.icon vc exportCSVModel
                        )
                    |> Maybe.withDefault none
            }
        }


view : View.Config -> String -> FilterHeaderConfig msg -> Model -> Html msg
view vc net config (Internal model) =
    div
        [ css [ Css.position Css.relative, Css.width <| Css.pct 100 ] ]
        [ filterHeader vc config (Internal model)
        , if model.showDialog then
            div
                [ [ Css.position Css.fixed
                  , Css.right (Css.px model.dialogPosition.right)
                  , Css.top (Css.px model.dialogPosition.top)
                  , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1000))
                  ]
                    |> css
                ]
                [ txFilterDialogView vc net config (Internal model)
                ]

          else
            none
        , model.tooltip
            |> Maybe.map
                (Html.text "tooltip"
                    |> flip (Tooltip.view (Pathfinder.tooltipConfig vc (TooltipMsg >> config.tag)))
                )
            |> Maybe.withDefault none
        ]


txFilterDialogView : View.Config -> String -> FilterHeaderConfig msg -> Model -> Html msg
txFilterDialogView vc net config (Internal model) =
    let
        toRadio name selected msg =
            Controls.radioSmall (Locale.string vc.locale name) selected msg

        isAssetFilterVisible =
            Data.isAccountLike net

        directionRadios =
            model.settings.direction
                |> Maybe.map
                    (\direction ->
                        [ TxTableFilterShowAllTxs |> toRadio "all transactions" (direction == Nothing)
                        , TxTableFilterShowIncomingTxOnly |> toRadio "incoming only" (direction == Just Incoming)
                        , TxTableFilterShowOutgoingTxOnly |> toRadio "outgoing only" (direction == Just Outgoing)
                        ]
                            |> List.map (Html.map config.tag)
                    )
                |> Maybe.withDefault []

        showQuickFilter =
            model.quickFilterSelect /= Nothing
    in
    SidePanelComponents.filterTransactionsPopupDevWithAttributes
        (SidePanelComponents.filterTransactionsPopupDevAttributes
            |> Rs.s_iconsCloseBlack [ Util.View.pointer, onClick (config.tag ToggleDialog) ]
            |> Rs.s_iconsInfoSnoPaddingDev
                (Tooltip.attributes (Pathfinder.tooltipConfig vc (TooltipMsg >> config.tag)))
            |> Rs.s_transactionDirection
                (if List.isEmpty directionRadios then
                    [ Css.display Css.none ] |> css |> List.singleton

                 else
                    []
                )
            |> Rs.s_zeroValue
                (if model.settings.includeZeroValueTxs |> Maybe.Extra.isJust then
                    []

                 else
                    [ Css.display Css.none ] |> css |> List.singleton
                )
            |> Rs.s_dateRange
                (if model.dateRangePicker |> Maybe.Extra.isJust then
                    []

                 else
                    [ Css.display Css.none ] |> css |> List.singleton
                )
            |> Rs.s_customFilterHeader
                [ UserClickedCustomFilterLabel
                    |> config.tag
                    |> onClick
                , pointer
                , css <|
                    if not showQuickFilter then
                        [ Css.display Css.none ]

                    else
                        []
                ]
            |> Rs.s_iconsDragHandle
                [ on "mousedown"
                    (Json.Decode.map2 (\x y -> config.tag <| StartDrag x y)
                        (Json.Decode.at [ "clientX" ] Json.Decode.int)
                        (Json.Decode.at [ "clientY" ] Json.Decode.int)
                    )
                , on "mouseup" (Json.Decode.succeed <| config.tag EndDrag)
                , css
                    [ Css.cursor <|
                        if model.isDragging then
                            Css.grabbing

                        else
                            Css.grab
                    ]
                ]
        )
        { radioItemsList = directionRadios
        }
        { cancelButton =
            { variant =
                Button.defaultConfig
                    |> Rs.s_text "reset"
                    |> Rs.s_onClick (Just ResetAllTxFilters)
                    |> Button.secondaryButton vc
                    |> Html.map config.tag
            }
        , confirmButton =
            { variant =
                Button.defaultConfig
                    |> Rs.s_text "done"
                    |> Rs.s_onClick (Just (config.tag ToggleDialog))
                    |> Button.primaryButton vc
            }
        , root =
            let
                drp =
                    SidePanelComponents.datePickerCtaWithAttributes
                        (SidePanelComponents.datePickerCtaAttributes
                            |> Rs.s_root
                                ([ Util.View.pointer
                                 , [ Css.hover SidePanelComponents.datePickerCtaStateHover_details.styles ] |> css
                                 ]
                                    ++ (case model.dateRangePicker of
                                            Just _ ->
                                                [ onClick OpenDateRangePicker ]

                                            Nothing ->
                                                []
                                       )
                                )
                        )
                        { root =
                            { placeholder = Locale.string vc.locale "select date range"
                            , state = SidePanelComponents.DatePickerCtaStateDefault
                            }
                        }
            in
            { dateInstance =
                Html.map config.tag <|
                    case model.dateRangePicker of
                        Just dmodel ->
                            let
                                startDate =
                                    dmodel.fromDate
                                        |> Maybe.map (renderDate vc (Locale.isFirstSecondOfTheDay vc.locale))

                                endDate =
                                    dmodel.toDate
                                        |> Maybe.map (renderDate vc (Locale.isLastSecondOfTheDay vc.locale))
                            in
                            if DatePicker.isOpen dmodel.dateRangePicker then
                                div []
                                    [ DateTimePicker.stylesheet
                                    , div [ css [ Css.fontSize (Css.px 12) ] ]
                                        [ DatePicker.view dmodel.settings dmodel.dateRangePicker
                                            |> Html.fromUnstyled
                                        ]
                                    ]

                            else
                                let
                                    drpFilledHeader =
                                        SidePanelComponents.datePickerFilledWithAttributes
                                            (SidePanelComponents.datePickerFilledAttributes
                                                |> Rs.s_root
                                                    ([ Util.View.pointer
                                                     , [ Css.hover SidePanelComponents.datePickerFilledStateHover_details.styles ] |> css
                                                     ]
                                                        ++ (case model.dateRangePicker of
                                                                Just _ ->
                                                                    [ onClick OpenDateRangePicker ]

                                                                Nothing ->
                                                                    []
                                                           )
                                                    )
                                                |> Rs.s_iconsCloseBlack
                                                    [ Json.Decode.succeed ( ResetDateRangePicker, True )
                                                        |> stopPropagationOn "click"
                                                    ]
                                            )
                                in
                                case ( startDate, endDate ) of
                                    ( Just startP, Just endP ) ->
                                        drpFilledHeader
                                            { root =
                                                { from = startP
                                                , to = endP
                                                , pronoun = Locale.string vc.locale "to"
                                                , state = SidePanelComponents.DatePickerFilledStateDefault
                                                , showIconClose = True
                                                }
                                            }

                                    ( Just startP, Nothing ) ->
                                        drpFilledHeader
                                            { root =
                                                { from = ""
                                                , to = startP
                                                , pronoun = Locale.string vc.locale "datefilter-starting"
                                                , state = SidePanelComponents.DatePickerFilledStateDefault
                                                , showIconClose = True
                                                }
                                            }

                                    ( Nothing, Just endP ) ->
                                        drpFilledHeader
                                            { root =
                                                { from = ""
                                                , to = endP
                                                , pronoun = Locale.string vc.locale "datefilter-until"
                                                , state = SidePanelComponents.DatePickerFilledStateDefault
                                                , showIconClose = True
                                                }
                                            }

                                    ( Nothing, Nothing ) ->
                                        drp

                        _ ->
                            drp
            , dateRangeLabel = Locale.string vc.locale "Date range"
            , headerTitle = Locale.string vc.locale "Transaction filter"
            , txDirectionLabel = Locale.string vc.locale "Transaction direction"
            , showAssetType = isAssetFilterVisible
            , assetDropdown =
                model.assetSelectBox
                    |> Maybe.map
                        (\sa ->
                            ThemedSelectBox.view
                                (ThemedSelectBox.defaultConfig (Maybe.withDefault (Locale.string vc.locale "All assets"))
                                    |> ThemedSelectBox.withAttributes [ css [ Css.width <| Css.px 200 ] ]
                                )
                                sa
                                model.settings.asset
                                |> Html.map TxTableAssetSelectBoxMsg
                                |> Html.map config.tag
                        )
                    |> Maybe.withDefault none
            , showQuickFilter = showQuickFilter
            , showCustomFilter = model.showCustomFilter || not showQuickFilter
            , customFilterLabel = Locale.string vc.locale "filter-custom-filter" |> Locale.titleCase vc.locale
            , quickFilterLabel = Locale.string vc.locale "Filter-quick-filter" |> Locale.titleCase vc.locale
            , showUtxoConstraint = not isAssetFilterVisible
            , quickfilterDropdown =
                model.quickFilterSelect
                    |> Maybe.map
                        (\qf ->
                            ThemedSelectBox.view
                                (ThemedSelectBox.defaultConfigHtml (quickFilterToLabel vc)
                                    |> ThemedSelectBox.withAttributes
                                        [ css
                                            [ Css.width <| Css.px 280
                                            , Css.height Css.auto
                                            ]
                                        ]
                                )
                                qf
                                (settingsToQuickFilter model)
                                |> Html.map TxTableQuickFilterSelectBoxMsg
                                |> Html.map config.tag
                        )
                    |> Maybe.withDefault none
            , zeroValuesLabel = Locale.string vc.locale "filter-label-exclude-zero-values"
            , followUtxoLabel = Locale.string vc.locale "filter-utxo-only"
            , assetTypeLabel = Locale.string vc.locale "Asset type"
            }
        , checkboxUtxoLevel =
            { variant =
                Checkbox.checkbox
                    { state =
                        if isAssetFilterVisible || settingsToQuickFilter model == Nothing then
                            Checkbox.disabledState

                        else
                            Internal model
                                |> getUtxoFilter
                                |> Maybe.map (\_ -> True)
                                |> Maybe.withDefault False
                                |> Checkbox.stateFromBool
                    , size = Checkbox.smallSize
                    , msg = Just (UserClickedUtxoOnly |> config.tag)
                    }
                    []
            }
        , customFilterChevron =
            { variant =
                Icons.iconsChevronRightThin
                    { root =
                        { state =
                            if model.showCustomFilter then
                                Icons.IconsChevronRightThinStateDown

                            else
                                Icons.IconsChevronRightThinStateDefault
                        }
                    }
            }
        , zeroValueSwitch =
            { variant =
                model.settings.includeZeroValueTxs
                    |> Maybe.map
                        (\selected ->
                            Controls.toggle
                                { size = Sc.SwitchSizeSmall
                                , disabled = False
                                , selected = not selected
                                , msg = TxTableFilterToggleZeroValue
                                }
                        )
                    |> Maybe.withDefault none
                    |> Html.map config.tag
            }
        }


settingsToQuickFilter : InternalModel -> Maybe QuickFilterModel
settingsToQuickFilter { settings, quickFilterSelect } =
    settings.range
        |> Maybe.Extra.join
        |> Maybe.Extra.andThen2
            (\dir r ->
                case ( dir, r ) of
                    ( Outgoing, Starting d ) ->
                        Just ( d, Outgoing )

                    ( Incoming, Until d ) ->
                        Just ( d, Incoming )

                    _ ->
                        Nothing
            )
            (Maybe.Extra.join settings.direction)
        |> Maybe.andThen
            (\( d, dir ) ->
                quickFilterSelect
                    |> Maybe.map ThemedSelectBox.getOptions
                    |> Maybe.withDefault []
                    |> List.filterMap identity
                    |> List.Extra.find
                        (\{ tx, direction, date } ->
                            (txToAsset tx == settings.asset)
                                && (direction == dir)
                                && (date == d)
                        )
            )


quickFilterToLabel : View.Config -> Maybe QuickFilterModel -> Html (ThemedSelectBox.Msg (Maybe QuickFilterModel))
quickFilterToLabel vc =
    Maybe.map
        (\qf ->
            Sc.filterGroupSmall
                { filterList =
                    quickfilterHeaderSmall vc qf
                        :: (qf.tx
                                |> txToAsset
                                |> Maybe.map (stringFilterSmall vc >> List.singleton)
                                |> Maybe.withDefault []
                           )
                }
                {}
        )
        >> Maybe.withDefault (Html.text <| Locale.string vc.locale "filter-none-selected")


quickfilterHeaderSmall : View.Config -> { a | direction : Direction, tx : Tx.TxType } -> Html msg
quickfilterHeaderSmall vc qf =
    let
        txLabel =
            case qf.direction of
                Outgoing ->
                    "Datefilter-starting-tx"

                Incoming ->
                    "Datefilter-until-tx"
    in
    qf.tx
        |> Tx.getRawBaseTxHashForTxType
        |> truncateLongIdentifier
        |> dateTimeFilterRawSmall vc txLabel


quickfilterHeader : View.Config -> { a | direction : Direction, tx : Tx.TxType } -> Html Msg
quickfilterHeader vc qf =
    let
        txLabel =
            case qf.direction of
                Outgoing ->
                    "Datefilter-starting-tx"

                Incoming ->
                    "Datefilter-until-tx"

        txHash =
            qf.tx
                |> Tx.getRawBaseTxHashForTxType
    in
    txHash
        |> truncateLongIdentifier
        |> dateTimeFilterRaw vc ResetAllTxFilters txLabel


init : Settings -> Model
init (Settings settings) =
    Internal
        { dateRangePicker = Nothing
        , assetSelectBox = Nothing
        , quickFilterSelect = Nothing
        , showCustomFilter = False
        , settings = settings
        , tooltip = Nothing
        , showDialog = False
        , dialogPosition = { top = 100, right = 20 }
        , isDragging = False
        , dragStart = Nothing
        }


initSettings : Settings
initSettings =
    Settings initSettingsModel


initSettingsFromQuickFilter : QuickFilter -> Settings
initSettingsFromQuickFilter (QuickFilterInternal qf) =
    qf
        |> quickFilterToSettings
        |> Settings


txToAsset : Tx.TxType -> Maybe String
txToAsset tx =
    case tx of
        Tx.Account t ->
            t.raw.currency
                |> String.toUpper
                |> Just

        Tx.Utxo _ ->
            Nothing


quickFilterToSettings : QuickFilterModel -> SettingsModel
quickFilterToSettings qf =
    { asset = txToAsset qf.tx
    , includeZeroValueTxs = Nothing
    , direction = Just <| Just qf.direction
    , range =
        Just <|
            Just <|
                case qf.direction of
                    Incoming ->
                        Until qf.date

                    Outgoing ->
                        Starting qf.date
    , utxoOnly = False
    }


initSettingsModel : SettingsModel
initSettingsModel =
    { direction = Nothing
    , asset = Nothing
    , includeZeroValueTxs = Nothing
    , range = Nothing
    , utxoOnly = False
    }


initQuickFilter : Tx.TxType -> Direction -> Posix -> QuickFilter
initQuickFilter tx dir date =
    QuickFilterInternal
        { direction = dir
        , date = date
        , tx = tx
        }


withDateRange : Posix -> Posix -> Settings -> Settings
withDateRange mn mx (Settings model) =
    Settings
        { model | range = Just <| Just <| Range mn mx }


withDateRangePicker : Locale.Model -> Posix -> Posix -> Model -> Model
withDateRangePicker locale mn mx (Internal model) =
    Internal
        { model
            | dateRangePicker =
                let
                    settings =
                        datePickerSettings locale mn mx

                    ( start, end ) =
                        Settings model.settings
                            |> getDateRange
                            |> Maybe.withDefault ( Nothing, Nothing )
                in
                model.dateRangePicker
                    |> Maybe.map (s_settings settings)
                    |> Maybe.withDefault
                        (settings
                            |> DateRangePicker.init UpdateDateRangePicker mx start end
                        )
                    |> Just
            , settings =
                model.settings
                    |> Rs.s_range
                        (model.settings.range
                            |> Maybe.withDefault Nothing
                            |> Just
                        )
        }


withQuickFilter : QuickFilter -> Model -> Model
withQuickFilter (QuickFilterInternal qf) (Internal model) =
    withQuickFilterInternal qf model
        |> Internal


withQuickFilterInternal : QuickFilterModel -> InternalModel -> InternalModel
withQuickFilterInternal _ model =
    {- model
       | quickFilterSelect =
           model.quickFilterSelect
               |> Maybe.withDefault
                   (ThemedSelectBox.init [ Nothing ])
               |> (\select ->
                       let
                           options =
                               ThemedSelectBox.getOptions select ++ [ Just qf ]
                       in
                       updateOptions options select
                  )
               |> Just
    -}
    model


updateOptions : List (Maybe QuickFilterModel) -> ThemedSelectBox.Model (Maybe QuickFilterModel) -> ThemedSelectBox.Model (Maybe QuickFilterModel)
updateOptions options select =
    let
        -- Create a unique key for each quick filter based on direction, date, tx hash, and currency
        quickFilterKey : Maybe QuickFilterModel -> String
        quickFilterKey opt =
            case opt of
                Nothing ->
                    "0"

                Just qf ->
                    let
                        txHash =
                            qf.tx |> Tx.getRawBaseTxHashForTxType

                        currency =
                            txToAsset qf.tx
                                |> Maybe.withDefault ""

                        directionStr =
                            case qf.direction of
                                Incoming ->
                                    "1"

                                Outgoing ->
                                    "0"

                        dateMillis =
                            qf.date |> Time.posixToMillis |> String.fromInt
                    in
                    String.join "|" [ directionStr, dateMillis, txHash, currency ]
    in
    options
        |> List.Extra.uniqueBy quickFilterKey
        |> List.sortBy quickFilterKey
        |> flip ThemedSelectBox.updateOptions select


withDirection : Maybe Direction -> Settings -> Settings
withDirection direction (Settings model) =
    Settings
        { model | direction = Just direction }


withAssetSelectBox : List String -> Model -> Model
withAssetSelectBox assets (Internal model) =
    Internal
        { model
            | assetSelectBox =
                let
                    options =
                        Nothing :: List.map Just assets
                in
                model.assetSelectBox
                    |> Maybe.map (ThemedSelectBox.updateOptions options)
                    |> Maybe.withDefault (ThemedSelectBox.init options)
                    |> Just
        }


withIncludeZeroValueTxs : Bool -> Settings -> Settings
withIncludeZeroValueTxs includeZeroValueTxs (Settings model) =
    Settings
        { model | includeZeroValueTxs = Just includeZeroValueTxs }


updateDateRange : ( Maybe Posix, Maybe Posix ) -> Model -> Model
updateDateRange range (Internal model) =
    updateDateRangeInternal range model
        |> Internal


updateDateRangeInternal : ( Maybe Posix, Maybe Posix ) -> InternalModel -> InternalModel
updateDateRangeInternal ( mn, mx ) model =
    { model
        | dateRangePicker =
            model.dateRangePicker
                |> Maybe.map (DateRangePicker.setFrom mn)
                |> Maybe.map (DateRangePicker.setTo mx)
        , settings =
            model.settings
                |> Rs.s_range
                    (model.settings.range
                        |> Maybe.map
                            (\_ ->
                                case ( mn, mx ) of
                                    ( Just a, Just b ) ->
                                        Range a b
                                            |> Just

                                    ( Just a, Nothing ) ->
                                        Starting a
                                            |> Just

                                    ( Nothing, Just b ) ->
                                        Until b
                                            |> Just

                                    ( Nothing, Nothing ) ->
                                        Nothing
                            )
                    )
    }


hasChanged : Model -> Model -> Bool
hasChanged (Internal old) (Internal new) =
    let
        newFromDate =
            new.dateRangePicker |> Maybe.map .fromDate

        oldFromDate =
            old.dateRangePicker |> Maybe.map .fromDate
    in
    (newFromDate /= oldFromDate)
        || (old.settings.asset /= new.settings.asset)
        || (old.settings.includeZeroValueTxs /= new.settings.includeZeroValueTxs)
        || (old.settings.direction /= new.settings.direction)
        || (old.settings.utxoOnly /= new.settings.utxoOnly)


setFocusDate : Time.Posix -> Model -> Model
setFocusDate focusDate (Internal model) =
    Internal
        { model
            | dateRangePicker =
                model.dateRangePicker
                    |> Maybe.map (DateRangePicker.setFocus focusDate)
        }


updateSelectedAsset : Maybe String -> Model -> Model
updateSelectedAsset selectedAsset (Internal model) =
    updateSelectedAssetInternal (Maybe.map String.toUpper selectedAsset) model
        |> Internal


updateSelectedAssetInternal : Maybe String -> InternalModel -> InternalModel
updateSelectedAssetInternal selectedAsset model =
    Rs.s_asset selectedAsset model.settings
        |> flip s_settings model


updateQuickFilters : List QuickFilter -> Model -> Model
updateQuickFilters quickFilters (Internal model) =
    quickFilters
        |> List.map
            (\qf ->
                case qf of
                    QuickFilterInternal qfModel ->
                        qfModel
            )
        |> List.foldl withQuickFilterInternal
            { model
                | quickFilterSelect =
                    Maybe.map (updateOptions [ Nothing ]) model.quickFilterSelect
            }
        |> Internal


getDirection : Settings -> Maybe Direction
getDirection (Settings model) =
    model.direction |> Maybe.Extra.join


getSelectedQuickFilter : Model -> Maybe QuickFilter
getSelectedQuickFilter (Internal model) =
    settingsToQuickFilter model
        |> Maybe.map QuickFilterInternal


applyQuickFilter : QuickFilterModel -> InternalModel -> InternalModel
applyQuickFilter qf model =
    let
        asset =
            txToAsset qf.tx
    in
    updateSelectedAssetInternal asset model
        |> updateDateRangeInternal
            (case qf.direction of
                Outgoing ->
                    ( Just qf.date, Nothing )

                Incoming ->
                    ( Nothing, Just qf.date )
            )
        |> updateDirectionInternal (Just qf.direction)


setSelectedQuickFilter : QuickFilter -> Model -> Model
setSelectedQuickFilter (QuickFilterInternal qf) (Internal model) =
    applyQuickFilter qf model
        |> Internal


getSettings : Model -> Settings
getSettings (Internal { settings }) =
    Settings settings


getUtxoFilter : Model -> Maybe ( Tx.UtxoTx, Direction )
getUtxoFilter (Internal model) =
    settingsToQuickFilter model
        |> Maybe.andThen
            (\qf ->
                case ( qf.tx, model.settings.utxoOnly ) of
                    ( Tx.Utxo utxo, True ) ->
                        Just ( utxo, qf.direction )

                    _ ->
                        Nothing
            )


getTx : QuickFilter -> Tx.TxType
getTx (QuickFilterInternal { tx }) =
    tx


getDirectionFromQuickFilter : QuickFilter -> Direction
getDirectionFromQuickFilter (QuickFilterInternal { direction }) =
    direction


subscriptions : Model -> Sub Msg
subscriptions (Internal model) =
    if model.isDragging then
        Browser.Events.onMouseMove
            (Json.Decode.map2 Drag
                (Json.Decode.at [ "clientX" ] Json.Decode.int)
                (Json.Decode.at [ "clientY" ] Json.Decode.int)
            )

    else
        Sub.none


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        TooltipEffect e ->
            Tooltip.perform e
                |> Cmd.map TooltipMsg
