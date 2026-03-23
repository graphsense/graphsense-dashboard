module View.Pathfinder.TransactionFilter exposing (FilterHeaderConfig, Model, Msg(..), filterHeader, getDateRange, getDirection, getIncludeZeroValueTxs, getSelectedAsset, hasChanged, init, setFocusDate, txFilterDialogView, update, updateDateRange, updateSelectedAsset, withAssetSelectBox, withDateRangePicker, withDirection, withIncludeZeroValueTxs)

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
import RecordSetter as Rs
import Svg.Styled.Attributes exposing (css)
import Theme.Colors
import Theme.Html.Icons as HIcons
import Theme.Html.SelectionControls as Sc
import Theme.Html.SidePanelComponents as SidePanelComponents
import Time exposing (Posix)
import Update.DateRangePicker as DateRangePicker
import Util.Css
import Util.Data as Data
import Util.ThemedSelectBox as ThemedSelectBox
import Util.View exposing (fullWidthCss, none)
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
                model.direction
                    |> Maybe.map (\_ -> { model | direction = Nothing })
                    |> Maybe.withDefault model

            TxTableFilterShowIncomingTxOnly ->
                model.direction
                    |> Maybe.map (\_ -> { model | direction = Just <| Just Incoming })
                    |> Maybe.withDefault model

            TxTableFilterShowOutgoingTxOnly ->
                model.direction
                    |> Maybe.map (\_ -> { model | direction = Just <| Just Outgoing })
                    |> Maybe.withDefault model

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
    HIcons.iconsCloseBlackWithAttributes
        (HIcons.iconsCloseBlackAttributes
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
        renderDate showTimeFn date =
            date
                |> (if showTimeFn date then
                        Locale.timestampDateUniform vc.locale

                    else
                        Locale.timestampDateTimeUniform vc.locale False
                   )

        startDate =
            dmodel.fromDate
                |> Maybe.map (renderDate (Locale.isFirstSecondOfTheDay vc.locale))

        endDate =
            dmodel.toDate
                |> Maybe.map (renderDate (Locale.isLastSecondOfTheDay vc.locale))
    in
    Maybe.map2
        (\startP endP ->
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
        )
        startDate
        endDate
        |> Maybe.withDefault none


directionFilterHeader : View.Config -> msg -> Direction -> Html msg
directionFilterHeader vc resetMsg dir =
    stringFilterHeader vc
        resetMsg
        (case dir of
            Incoming ->
                "incoming only"

            Outgoing ->
                "outgoing only"
        )


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
            { iconInstance = HIcons.iconsFilter {}
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
    in
    SidePanelComponents.filterTransactionsPopupWithAttributes
        (SidePanelComponents.filterTransactionsPopupAttributes
            |> Rs.s_assetType
                (if isAssetFilterVisible then
                    [ Css.padding (Css.px 0) |> Css.important ] |> css |> List.singleton

                 else
                    [ Css.display Css.none ] |> css |> List.singleton
                )
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
        )
        { radioItemsList = directionRadios
        }
        { assetType = { label = "" }
        , cancelButton =
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
        , dropDown =
            { variant =
                model.assetSelectBox
                    |> Maybe.map
                        (\sa ->
                            ThemedSelectBox.viewWithLabel
                                (ThemedSelectBox.defaultConfig (Maybe.withDefault (Locale.string vc.locale "All assets"))
                                    |> Rs.s_width (Just (Css.px 200))
                                )
                                sa
                                model.selectedAsset
                                (Locale.string vc.locale "Asset type")
                                |> Html.map TxTableAssetSelectBoxMsg
                                |> Html.map config.tag
                        )
                    |> Maybe.withDefault none
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
                                        (Locale.timestampDateUniform vc.locale)

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
                                Maybe.map2
                                    (\startP endP ->
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
                                            { root = { from = startP, to = endP, pronoun = Locale.string vc.locale "to", state = SidePanelComponents.DatePickerFilledStateDefault } }
                                    )
                                    startDate
                                    endDate
                                    |> Maybe.withDefault drp

                        _ ->
                            drp
            , dateLabel = Locale.string vc.locale "Date range"
            , headerTitle = Locale.string vc.locale "Transaction filter"
            , txDirection = Locale.string vc.locale "Transaction direction"
            , zeroValues = Locale.string vc.locale "Exclude zero value transfers"
            }
        , switch =
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


init : Model
init =
    Internal
        { dateRangePicker = Nothing
        , direction = Nothing
        , assetSelectBox = Nothing
        , selectedAsset = Nothing
        , includeZeroValueTxs = Nothing
        }


withDateRangePicker : Locale.Model -> Posix -> Posix -> Model -> Model
withDateRangePicker locale mn mx (Internal builder) =
    Internal
        { builder
            | dateRangePicker =
                datePickerSettings locale mn mx
                    |> DateRangePicker.init UpdateDateRangePicker mx Nothing Nothing
                    |> Just
        }


withDirection : Maybe Direction -> Model -> Model
withDirection direction (Internal builder) =
    Internal
        { builder | direction = Just direction }


withAssetSelectBox : List String -> Model -> Model
withAssetSelectBox assets (Internal builder) =
    Internal
        { builder | assetSelectBox = Just (ThemedSelectBox.init (Nothing :: List.map Just assets)) }


withIncludeZeroValueTxs : Bool -> Model -> Model
withIncludeZeroValueTxs includeZeroValueTxs (Internal builder) =
    Internal
        { builder | includeZeroValueTxs = Just includeZeroValueTxs }


updateDateRange : ( Maybe Posix, Maybe Posix ) -> Model -> Model
updateDateRange ( mn, mx ) (Internal model) =
    Internal
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
    Internal
        { model | selectedAsset = selectedAsset }


getDirection : Model -> Maybe Direction
getDirection (Internal model) =
    model.direction |> Maybe.Extra.join
