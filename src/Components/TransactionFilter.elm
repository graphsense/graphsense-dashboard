module Components.TransactionFilter exposing (FilterHeaderConfig, Model, Msg(..), applyQuickFilter, filterHeader, getDateRange, getDirection, getIncludeZeroValueTxs, getSelectedAsset, hasChanged, init, initQuickFilter, quickfilterWithAsset, setFocusDate, setSelectedQuickFilter, txFilterDialogView, update, updateDateRange, updateDirection, updateSelectedAsset, withAssetSelectBox, withDateRangePicker, withDirection, withIncludeZeroValueTxs, withQuickFilter)

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
import Maybe.Extra
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import RecordSetter as Rs exposing (s_settings)
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
import Util.View exposing (fullWidthCss, none, pointer)
import View.Button as Button
import View.Controls as Controls
import View.Locale as Locale


type Model
    = Internal InternalModel


type alias InternalModel =
    { dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , direction : Maybe (Maybe Direction)
    , selectedAsset : Maybe String
    , assetSelectBox : Maybe (ThemedSelectBox.Model (Maybe String))
    , includeZeroValueTxs : Maybe Bool
    , quickFilterSelect : Maybe (ThemedSelectBox.Model (Maybe QuickFilterModel))
    , selectedQuickFilter : Maybe QuickFilterModel
    , showCustomFilter : Bool
    }


type QuickFilter
    = QuickFilterInternal QuickFilterModel


type alias QuickFilterModel =
    { asset : Maybe String
    , date : Posix
    , direction : Direction
    }


getDateRange : Model -> Maybe ( Maybe Posix, Maybe Posix )
getDateRange (Internal model) =
    model.dateRangePicker
        |> Maybe.map (\drp -> ( drp.fromDate, drp.toDate ))


getSelectedAsset : Model -> Maybe String
getSelectedAsset (Internal model) =
    model.selectedAsset


getIncludeZeroValueTxs : Model -> Maybe Bool
getIncludeZeroValueTxs (Internal model) =
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
                            in
                            { model
                                | dateRangePicker = Just newPicker
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
                { model
                    | includeZeroValueTxs =
                        model.includeZeroValueTxs
                            |> Maybe.withDefault False
                            |> not
                            |> Just
                }

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
                                , selectedAsset =
                                    case outMsg of
                                        ThemedSelectBox.Selected sel ->
                                            sel

                                        _ ->
                                            model.selectedAsset
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

                                selectedQuickFilter =
                                    case outMsg of
                                        ThemedSelectBox.Selected sel ->
                                            sel

                                        _ ->
                                            model.selectedQuickFilter
                            in
                            { model
                                | quickFilterSelect = Just newSelect
                                , selectedQuickFilter = selectedQuickFilter
                            }
                                |> (selectedQuickFilter
                                        |> Maybe.map applyQuickFilter
                                        |> Maybe.withDefault identity
                                   )
                        )
                    |> Maybe.withDefault model

            UserClickedCustomFilterLabel ->
                { model | showCustomFilter = not model.showCustomFilter }


updateDirectionInternal : Maybe Direction -> InternalModel -> InternalModel
updateDirectionInternal direction model =
    model.direction
        |> Maybe.map (\_ -> { model | direction = Just direction })
        |> Maybe.withDefault model


updateDirection : Maybe Direction -> Model -> Model
updateDirection direction (Internal model) =
    updateDirectionInternal direction model
        |> Internal


resetIncludeZeroValueTxs : InternalModel -> InternalModel
resetIncludeZeroValueTxs model =
    { model
        | includeZeroValueTxs =
            model.includeZeroValueTxs
                |> Maybe.map (\_ -> True)
    }


resetSelectedAsset : InternalModel -> InternalModel
resetSelectedAsset model =
    { model
        | selectedAsset = Nothing
    }


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
    { model
        | direction =
            model.direction
                |> Maybe.map (\_ -> Nothing)
    }


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
        dateRendered =
            date
                |> renderDate vc (Locale.isFirstSecondOfTheDay vc.locale)
    in
    case dir of
        Outgoing ->
            SidePanelComponents.filterLabelSmall
                { root =
                    { text1 = dateRendered
                    , text2 = Locale.string vc.locale "datefilter-starting"
                    , text3 = ""
                    , dateRangeVisible = True
                    }
                }

        Incoming ->
            SidePanelComponents.filterLabelSmall
                { root =
                    { text1 = dateRendered
                    , text2 = Locale.string vc.locale "datefilter-until"
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
            [ model.dateRangePicker |> Maybe.map (dateTimeFilterHeader vc ResetDateRangePicker)
            , model.direction |> Maybe.Extra.join |> Maybe.map (directionFilterHeader vc ResetTxDirectionFilter)
            , model.selectedAsset |> Maybe.map (assetFilterHeader vc ResetTxAssetFilter)
            , model.includeZeroValueTxs |> Maybe.map (zeroValuesHeader vc ResetZeroValueSubTxsTableFilters)
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
            model.direction
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
                (if model.includeZeroValueTxs |> Maybe.Extra.isJust then
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
                                prepDate =
                                    Maybe.map
                                        (Locale.timestampDateTimeUniform vc.locale False)

                                startDate =
                                    dmodel.fromDate
                                        |> prepDate

                                endDate =
                                    dmodel.toDate
                                        |> prepDate
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
                                model.selectedAsset
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
                                model.selectedQuickFilter
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
                model.includeZeroValueTxs
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
                        ++ [ qf.direction |> directionFilterString |> stringFilterSmall vc
                           , qf.date |> dateTimeFilterSmall vc qf.direction
                           ]
                }
                {}
        )
        >> Maybe.withDefault (Html.text <| Locale.string vc.locale "filter-none-selected")


init : Model
init =
    Internal
        { dateRangePicker = Nothing
        , direction = Nothing
        , assetSelectBox = Nothing
        , selectedAsset = Nothing
        , includeZeroValueTxs = Nothing
        , quickFilterSelect = Nothing
        , selectedQuickFilter = Nothing
        , showCustomFilter = False
        }


initQuickFilter : Direction -> Posix -> QuickFilter
initQuickFilter dir date =
    QuickFilterInternal
        { direction = dir
        , date = date
        , asset = Nothing
        }


quickfilterWithAsset : String -> QuickFilter -> QuickFilter
quickfilterWithAsset asset (QuickFilterInternal qf) =
    QuickFilterInternal { qf | asset = Just asset }


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


withDirection : Maybe Direction -> Model -> Model
withDirection direction (Internal model) =
    Internal
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


withIncludeZeroValueTxs : Bool -> Model -> Model
withIncludeZeroValueTxs includeZeroValueTxs (Internal model) =
    Internal
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
        || (old.selectedAsset /= new.selectedAsset)
        || (old.includeZeroValueTxs /= new.includeZeroValueTxs)
        || (old.direction /= new.direction)


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
    { model | selectedAsset = selectedAsset }


getDirection : Model -> Maybe Direction
getDirection (Internal model) =
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
    Internal
        { model | selectedQuickFilter = Just qf }
