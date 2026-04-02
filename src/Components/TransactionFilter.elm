module Components.TransactionFilter exposing (FilterHeaderConfig, Model, Msg(..), Settings, applyQuickFilter, filterHeader, getDateRange, getDirection, getIncludeZeroValueTxs, getSelectedAsset, getSettings, hasChanged, init, initQuickFilter, initSettings, initSettingsFromQuickFilter, quickfilterWithAsset, setFocusDate, setSelectedQuickFilter, txFilterDialogView, update, updateDateRange, updateDirection, updateSelectedAsset, withAssetSelectBox, withDateRange, withDateRangePicker, withDirection, withIncludeZeroValueTxs, withQuickFilter)

import Basics.Extra exposing (flip)
import Components.ExportCSV as ExportCSV
import Config.DateRangePicker exposing (datePickerSettings)
import Config.View as View
import Css
import Css.DateTimePicker as DateTimePicker
import DurationDatePicker as DatePicker
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Init.DateRangePicker as DateRangePicker
import List.Extra
import Maybe.Extra
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import RecordSetter as Rs exposing (s_direction, s_settings)
import Svg.Styled.Attributes exposing (css)
import Theme.Colors
import Theme.Html.Icons as Icons
import Theme.Html.SelectionControls as Sc
import Theme.Html.SidePanelComponents as SidePanelComponents
import Time exposing (Posix)
import Update.DateRangePicker as DateRangePicker
import Util.Checkbox as Checkbox
import Util.Css
import Util.Data as Data
import Util.ThemedSelectBox as ThemedSelectBox
import Util.View exposing (fullWidthCss, none, pointer, truncateLongIdentifier)
import View.Button as Button
import View.Controls as Controls
import View.Locale as Locale


type Model
    = Internal InternalModel


type alias InternalModel =
    { dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , assetSelectBox : Maybe (ThemedSelectBox.Model (Maybe String))
    , quickFilterSelect : Maybe (ThemedSelectBox.Model (Maybe QuickFilterModel))
    , showCustomFilter : Bool
    , settings : SettingsModel
    }


type Settings
    = Settings SettingsModel


type alias SettingsModel =
    { range : Maybe (Maybe Range)
    , asset : Maybe String
    , direction : Maybe (Maybe Direction)
    , includeZeroValueTxs : Maybe Bool
    }


type Range
    = Starting Posix
    | Until Posix
    | Range Posix Posix


type QuickFilter
    = QuickFilterInternal QuickFilterModel


type alias QuickFilterModel =
    { asset : Maybe String
    , date : Posix
    , direction : Direction
    , txHash : String
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
    , toggleTxFilterViewMsg : msg
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


update : Msg -> Model -> Model
update msg (Internal model) =
    Internal <|
        case msg of
            ResetAllTxFilters ->
                resetSelectedAsset model
                    |> resetDateRangePicker
                    |> resetDirection
                    |> resetIncludeZeroValueTxs

            ResetDateRangePicker ->
                resetDateRangePicker model

            ResetTxDirectionFilter ->
                resetDirection model

            ResetTxAssetFilter ->
                resetSelectedAsset model

            ResetZeroValueSubTxsTableFilters ->
                resetIncludeZeroValueTxs model

            OpenDateRangePicker ->
                { model | dateRangePicker = Maybe.map DateRangePicker.openPicker model.dateRangePicker }

            CloseDateRangePicker ->
                { model | dateRangePicker = Maybe.map DateRangePicker.closePicker model.dateRangePicker }

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
                        )
                    |> Maybe.withDefault model

            TxTableFilterShowAllTxs ->
                updateDirectionInternal Nothing model

            TxTableFilterShowIncomingTxOnly ->
                updateDirectionInternal (Just Incoming) model

            TxTableFilterShowOutgoingTxOnly ->
                updateDirectionInternal (Just Incoming) model

            TxTableFilterToggleZeroValue ->
                model.settings.includeZeroValueTxs
                    |> Maybe.map not
                    |> flip Rs.s_includeZeroValueTxs model.settings
                    |> flip s_settings model

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
                                                |> Maybe.withDefault identity

                                        _ ->
                                            identity
                                   )
                        )
                    |> Maybe.withDefault model

            UserClickedCustomFilterLabel ->
                { model | showCustomFilter = not model.showCustomFilter }


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
    }


resetDirection : InternalModel -> InternalModel
resetDirection model =
    model.settings.direction
        |> Maybe.map (\_ -> Nothing)
        |> flip s_direction model.settings
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


dateTimeFilterHeader : View.Config -> Msg -> DateRangePicker.Model Msg -> Html Msg
dateTimeFilterHeader vc resetMsg dmodel =
    let
        startDate =
            dmodel.fromDate
                |> Maybe.map (renderDate vc (Locale.isFirstSecondOfTheDay vc.locale))

        endDate =
            dmodel.toDate
                |> Maybe.map (renderDate vc (Locale.isLastSecondOfTheDay vc.locale))
    in
    renderDateTimeFilter vc resetMsg startDate endDate


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
                    , text1 = endP
                    , text2 = "-"
                    , text3 = startP
                    , dateRangeVisible = True
                    }
                }

        ( Just startP, Nothing ) ->
            SidePanelComponents.filterLabel
                { root =
                    { iconInstance =
                        closeButtonGrey resetMsg
                    , text1 = startP
                    , text2 = Locale.string vc.locale "datefilter-starting"
                    , text3 = ""
                    , dateRangeVisible = True
                    }
                }

        ( Nothing, Just endP ) ->
            SidePanelComponents.filterLabel
                { root =
                    { iconInstance =
                        closeButtonGrey resetMsg
                    , text1 = endP
                    , text2 = Locale.string vc.locale "datefilter-until"
                    , text3 = ""
                    , dateRangeVisible = True
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


dateTimeFilterSmall : View.Config -> Direction -> Posix -> Html msg
dateTimeFilterSmall vc dir date =
    let
        dirLabel =
            case dir of
                Outgoing ->
                    "datefilter-starting"

                Incoming ->
                    "datefilter-until"
    in
    date
        |> renderDate vc (Locale.isFirstSecondOfTheDay vc.locale)
        |> dateTimeFilterRawSmall vc dirLabel


dateTimeFilterRawSmall : View.Config -> String -> String -> Html msg
dateTimeFilterRawSmall vc label text =
    SidePanelComponents.filterLabelSmall
        { root =
            { text1 = text
            , text2 = Locale.string vc.locale label
            , text3 = ""
            , dateRangeVisible = True
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
            , text3 = ""
            , text2 = ""
            , text1 =
                Locale.string vc.locale str
            , dateRangeVisible = False
            }
        }


stringFilterSmall : View.Config -> String -> Html msg
stringFilterSmall vc str =
    SidePanelComponents.filterLabelSmall
        { root =
            { text3 = ""
            , text2 = ""
            , text1 =
                Locale.string vc.locale str
            , dateRangeVisible = False
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
        stringFilterHeader vc resetMsg "no zero value"


filterHeader : View.Config -> FilterHeaderConfig msg -> Model -> Html msg
filterHeader vc config (Internal model) =
    SidePanelComponents.sidePanelListFilterRowWithAttributes
        (SidePanelComponents.sidePanelListFilterRowAttributes
            |> Rs.s_root
                [ css [ fullWidthCss ]
                ]
            |> Rs.s_framedFilter
                [ onClick config.toggleTxFilterViewMsg
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
            [ model.settings.range |> Maybe.map (dateTimeFilterHeaderFromRange vc ResetDateRangePicker)
            , model.settings.direction |> Maybe.Extra.join |> Maybe.map (directionFilterHeader vc ResetTxDirectionFilter)
            , model.settings.asset |> Maybe.map (assetFilterHeader vc ResetTxAssetFilter)
            , model.settings.includeZeroValueTxs |> Maybe.map (zeroValuesHeader vc ResetZeroValueSubTxsTableFilters)
            ]
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
            |> Rs.s_iconsCloseBlack [ Util.View.pointer, onClick config.toggleTxFilterViewMsg ]
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
                    |> Rs.s_onClick (Just config.toggleTxFilterViewMsg)
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
                                            )
                                in
                                case ( startDate, endDate ) of
                                    ( Just startP, Just endP ) ->
                                        drpFilledHeader
                                            { root = { from = startP, to = endP, pronoun = Locale.string vc.locale "to", state = SidePanelComponents.DatePickerFilledStateDefault } }

                                    ( Just startP, Nothing ) ->
                                        drpFilledHeader
                                            { root = { from = "", to = startP, pronoun = Locale.string vc.locale "datefilter-starting", state = SidePanelComponents.DatePickerFilledStateDefault } }

                                    ( Nothing, Just endP ) ->
                                        drpFilledHeader
                                            { root = { from = "", to = endP, pronoun = Locale.string vc.locale "datefilter-until", state = SidePanelComponents.DatePickerFilledStateDefault } }

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
                            ThemedSelectBox.viewWithLabel
                                (ThemedSelectBox.defaultConfig (Maybe.withDefault (Locale.string vc.locale "All assets")))
                                [ css [ Css.width <| Css.px 200 ] ]
                                sa
                                model.settings.asset
                                (Locale.string vc.locale "Asset type")
                                |> Html.map TxTableAssetSelectBoxMsg
                                |> Html.map config.tag
                        )
                    |> Maybe.withDefault none
            , showQuickFilter = showQuickFilter
            , showCustomFilter = model.showCustomFilter || not showQuickFilter
            , customFilterLabel = Locale.string vc.locale "filter-custom-filter" |> Locale.titleCase vc.locale
            , quickFilterLabel = Locale.string vc.locale "filter-quick-filter" |> Locale.titleCase vc.locale
            , showUtxoConstraint = not isAssetFilterVisible
            , quickfilterDropdown =
                model.quickFilterSelect
                    |> Maybe.map
                        (\qf ->
                            ThemedSelectBox.view
                                (ThemedSelectBox.defaultConfigHtml (quickFilterToLabel vc))
                                [ css
                                    [ Css.width <| Css.px 250
                                    , Css.height Css.auto
                                    ]
                                ]
                                qf
                                (settingsToQuickFilter model)
                                |> Html.map TxTableQuickFilterSelectBoxMsg
                                |> Html.map config.tag
                        )
                    |> Maybe.withDefault none
            , zeroValuesLabel = Locale.string vc.locale "Exclude zero value transfers"
            , followUtxoLabel = Locale.string vc.locale "filter-follow-utxo"
            , assetTypeLabel = Locale.string vc.locale "Asset type"
            }
        , checkboxUtxoLevel =
            { variant =
                Checkbox.checkbox
                    { state = Checkbox.stateFromBool False
                    , size = Checkbox.smallSize
                    , msg = Nothing
                    }
                    []
            }
        , customFilterChevron =
            { variant =
                Icons.chevron
                    { root =
                        { state =
                            if model.showCustomFilter then
                                Icons.ChevronStateDown

                            else
                                Icons.ChevronStateDefault
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
                        (\{ asset, direction, date } ->
                            asset
                                == settings.asset
                                && direction
                                == dir
                                && date
                                == d
                        )
            )



{--asset = settings.asset
                , date = d
                , direction = dir
                --}


quickFilterToLabel : View.Config -> Maybe QuickFilterModel -> Html (ThemedSelectBox.Msg (Maybe QuickFilterModel))
quickFilterToLabel vc =
    Maybe.map
        (\qf ->
            Sc.filterGroupSmall
                { filterList =
                    (qf.asset
                        |> Maybe.map (stringFilterSmall vc >> List.singleton)
                        |> Maybe.withDefault []
                    )
                        ++ [ qf.date |> dateTimeFilterSmall vc qf.direction
                           , qf.txHash
                                |> truncateLongIdentifier
                                |> dateTimeFilterRawSmall vc "by Tx"
                           , qf.direction |> directionFilterString |> stringFilterSmall vc
                           ]
                }
                {}
        )
        >> Maybe.withDefault (Html.text <| Locale.string vc.locale "filter-none-selected")


init : Settings -> Model
init (Settings settings) =
    Internal
        { dateRangePicker = Nothing
        , assetSelectBox = Nothing
        , quickFilterSelect = Nothing
        , showCustomFilter = False
        , settings = settings
        }


initSettings : Settings
initSettings =
    Settings initSettingsModel


initSettingsFromQuickFilter : QuickFilter -> Settings
initSettingsFromQuickFilter (QuickFilterInternal qf) =
    qf
        |> quickFilterToSettings
        |> Debug.log "qf"
        |> Settings


quickFilterToSettings : QuickFilterModel -> SettingsModel
quickFilterToSettings qf =
    { asset = qf.asset
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
    }


initSettingsModel : SettingsModel
initSettingsModel =
    { direction = Nothing
    , asset = Nothing
    , includeZeroValueTxs = Nothing
    , range = Nothing
    }


initQuickFilter : String -> Direction -> Posix -> QuickFilter
initQuickFilter txHash dir date =
    QuickFilterInternal
        { direction = dir
        , date = date
        , asset = Nothing
        , txHash = txHash
        }


quickfilterWithAsset : String -> QuickFilter -> QuickFilter
quickfilterWithAsset asset (QuickFilterInternal qf) =
    QuickFilterInternal { qf | asset = Just asset }


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
                in
                model.dateRangePicker
                    |> Maybe.map (s_settings settings)
                    |> Maybe.withDefault
                        (settings
                            |> DateRangePicker.init UpdateDateRangePicker mx Nothing Nothing
                        )
                    |> Just
        }


withQuickFilter : QuickFilter -> Model -> Model
withQuickFilter (QuickFilterInternal qf) (Internal model) =
    Internal
        { model
            | quickFilterSelect =
                model.quickFilterSelect
                    |> Maybe.withDefault
                        (ThemedSelectBox.init [ Nothing ])
                    |> (\select ->
                            let
                                options =
                                    ThemedSelectBox.getOptions select ++ [ Just qf ]
                            in
                            ThemedSelectBox.updateOptions options select
                       )
                    |> Just
        }


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
    updateSelectedAssetInternal selectedAsset model
        |> Internal


updateSelectedAssetInternal : Maybe String -> InternalModel -> InternalModel
updateSelectedAssetInternal selectedAsset model =
    Rs.s_asset selectedAsset model.settings
        |> flip s_settings model


getDirection : Settings -> Maybe Direction
getDirection (Settings model) =
    model.direction |> Maybe.Extra.join


applyQuickFilter : QuickFilterModel -> InternalModel -> InternalModel
applyQuickFilter qf model =
    updateSelectedAssetInternal qf.asset model
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
