module View.Pathfinder.TransactionFilter exposing (FilterDialogConfig, FilterHeaderConfig, FilterMetadata, filterHeader, txFilterDialogView)

import Config.View as View
import Css
import Css.DateTimePicker as DateTimePicker
import DurationDatePicker as DatePicker
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import RecordSetter as Rs
import Svg.Styled.Attributes exposing (css)
import Theme.Colors
import Theme.Html.Icons as HIcons
import Theme.Html.SelectionControls as Sc
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Css
import Util.Data as Data
import Util.ThemedSelectBox as ThemedSelectBox
import Util.View exposing (none)
import View.Button as Button
import View.Controls as Controls
import View.Locale as Locale


type alias FilterMetadata msg x =
    { x
        | dateRangePicker : Maybe (DateRangePicker.Model msg)
        , direction : Maybe Direction
        , selectedAsset : Maybe String
        , assetSelectBox : ThemedSelectBox.Model (Maybe String)
        , includeZeroValueTxs : Maybe Bool
    }


type alias FilterHeaderConfig msg =
    { resetDateFilterMsg : msg
    , resetDirectionFilterMsg : Maybe msg
    , resetAssetsFilterMsg : msg
    , resetZeroValueFilterMsg : Maybe msg
    , toggleFilterView : msg
    }


type alias FilterDialogConfig msg =
    { closeTxFilterViewMsg : msg
    , txTableFilterShowAllTxsMsg : Maybe msg
    , txTableFilterShowIncomingTxOnlyMsg : Maybe msg
    , txTableFilterShowOutgoingTxOnlyMsg : Maybe msg
    , txTableFilterToggleZeroValueMsg : Maybe msg
    , resetAllTxFiltersMsg : msg
    , txTableAssetSelectBoxMsg : ThemedSelectBox.Msg (Maybe String) -> msg
    , openDateRangePickerMsg : Maybe msg
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


dateTimeFilterHeader : View.Config -> msg -> DateRangePicker.Model msg -> Html msg
dateTimeFilterHeader vc resetMsg dmodel =
    let
        renderDate showTimeFn date =
            date
                |> Locale.posixToTimestampSeconds
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
                    , text2 = Locale.string vc.locale "to"
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
                "Incoming only"

            Outgoing ->
                "Outgoing only"
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
assetFilterHeader vc resetMsg asset =
    stringFilterHeader vc resetMsg asset


zeroValuesHeader : View.Config -> msg -> Bool -> Html msg
zeroValuesHeader vc resetMsg includeZeroValueTxs =
    if includeZeroValueTxs then
        none

    else
        stringFilterHeader vc resetMsg "No zero value"


filterHeader : View.Config -> FilterMetadata msg x -> FilterHeaderConfig msg -> Html msg
filterHeader vc model config =
    div
        [ [ Css.displayFlex
          , Css.justifyContent Css.spaceBetween
          , Css.padding (Css.px 10)
          , Css.property "gap" "5px"
          ]
            |> css
        ]
        [ div
            [ [ Css.displayFlex
              , Css.flexDirection Css.row
              , Css.property "gap" "5px"
              , Css.flexWrap Css.wrap
              , Css.width (Css.px 320)
              ]
                |> css
            ]
            [ model.dateRangePicker |> Maybe.map (dateTimeFilterHeader vc config.resetDateFilterMsg) |> Maybe.withDefault none
            , case config.resetDirectionFilterMsg of
                Just rdmsg ->
                    model.direction |> Maybe.map (directionFilterHeader vc rdmsg) |> Maybe.withDefault none

                _ ->
                    none
            , model.selectedAsset |> Maybe.map (assetFilterHeader vc config.resetAssetsFilterMsg) |> Maybe.withDefault none
            , model.includeZeroValueTxs |> Maybe.map2 (\b m -> zeroValuesHeader vc b m) config.resetZeroValueFilterMsg |> Maybe.withDefault none
            ]
        , div []
            [ HIcons.framedIconWithAttributes
                (HIcons.framedIconAttributes
                    |> Rs.s_root
                        [ onClick config.toggleFilterView
                        , Util.View.pointer
                        ]
                )
                { root = { iconInstance = HIcons.iconsFilter {} } }
            ]
        ]


txFilterDialogView : View.Config -> String -> FilterDialogConfig msg -> FilterMetadata msg x -> Html msg
txFilterDialogView vc net config model =
    let
        toRadio name selected msg =
            Controls.radioSmall (Locale.string vc.locale name) selected msg

        isAssetFilterVisible =
            Data.isAccountLike net

        directionRadios =
            [ config.txTableFilterShowAllTxsMsg |> Maybe.map (toRadio "All transactions" (model.direction == Nothing))
            , config.txTableFilterShowIncomingTxOnlyMsg |> Maybe.map (toRadio "Incoming only" (model.direction == Just Incoming))
            , config.txTableFilterShowOutgoingTxOnlyMsg |> Maybe.map (toRadio "Outgoing only" (model.direction == Just Outgoing))
            ]
                |> List.filterMap identity
    in
    SidePanelComponents.filterTransactionsPopupWithAttributes
        (SidePanelComponents.filterTransactionsPopupAttributes
            |> Rs.s_assetType
                (if isAssetFilterVisible then
                    [ Css.padding (Css.px 0) |> Css.important ] |> css |> List.singleton

                 else
                    [ Css.display Css.none ] |> css |> List.singleton
                )
            |> Rs.s_iconsCloseBlack [ Util.View.pointer, onClick config.closeTxFilterViewMsg ]
            |> Rs.s_transactionDirection
                (if List.isEmpty directionRadios then
                    [ Css.display Css.none ] |> css |> List.singleton

                 else
                    []
                )
            |> Rs.s_zeroValue
                (if config.txTableFilterToggleZeroValueMsg |> Maybe.Extra.isJust then
                    []

                 else
                    [ Css.display Css.none ] |> css |> List.singleton
                )
            |> Rs.s_dateRange
                (if config.openDateRangePickerMsg |> Maybe.Extra.isJust then
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
                    |> Rs.s_text "Reset"
                    |> Rs.s_onClick (Just config.resetAllTxFiltersMsg)
                    |> Button.secondaryButton vc
            }
        , confirmButton =
            { variant =
                Button.defaultConfig
                    |> Rs.s_text "Done"
                    |> Rs.s_onClick (Just config.closeTxFilterViewMsg)
                    |> Button.primaryButton vc
            }
        , dropDown =
            { variant =
                if isAssetFilterVisible then
                    ThemedSelectBox.viewWithLabel (ThemedSelectBox.defaultConfig (Maybe.withDefault "All assets") |> Rs.s_width (Just (Css.px 200))) model.assetSelectBox model.selectedAsset (Locale.string vc.locale "Asset Type")
                        |> Html.map config.txTableAssetSelectBoxMsg

                else
                    none
            }
        , root =
            { dateInstance =
                case model.dateRangePicker of
                    Just dmodel ->
                        let
                            prepDate =
                                Maybe.map
                                    (Locale.posixToTimestampSeconds
                                        >> Locale.timestampDateUniform vc.locale
                                    )

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
                                                    ++ (case config.openDateRangePickerMsg of
                                                            Just msg ->
                                                                [ onClick msg ]

                                                            Nothing ->
                                                                []
                                                       )
                                                )
                                        )
                                        { root = { from = startP, to = endP, pronoun = Locale.string vc.locale "to", state = SidePanelComponents.DatePickerFilledStateDefault } }
                                )
                                startDate
                                endDate
                                |> Maybe.withDefault none

                    _ ->
                        SidePanelComponents.datePickerCtaWithAttributes
                            (SidePanelComponents.datePickerCtaAttributes
                                |> Rs.s_root
                                    ([ Util.View.pointer
                                     , [ Css.hover SidePanelComponents.datePickerCtaStateHover_details.styles ] |> css
                                     ]
                                        ++ (case config.openDateRangePickerMsg of
                                                Just msg ->
                                                    [ onClick msg ]

                                                Nothing ->
                                                    []
                                           )
                                    )
                            )
                            { root =
                                { placeholder = Locale.string vc.locale "Select date range"
                                , state = SidePanelComponents.DatePickerCtaStateDefault
                                }
                            }
            , dateLabel = Locale.string vc.locale "Date Range"
            , headerTitle = Locale.string vc.locale "Transaction Filter"
            , txDirection = Locale.string vc.locale "Transaction Direction"
            , zeroValues = Locale.string vc.locale "Exclude Zero Value Transfers"
            }
        , switch =
            { variant =
                config.txTableFilterToggleZeroValueMsg
                    |> Maybe.map
                        (\msg ->
                            Controls.toggle
                                { size = Sc.SwitchSizeSmall
                                , disabled = False
                                , selected = model.includeZeroValueTxs |> Maybe.withDefault True |> not
                                , msg = msg
                                }
                        )
                    |> Maybe.withDefault none
            }
        }
