module View.Settings exposing (view)

-- import Config.Graph as Graph
import Config.View exposing (Config)
-- import Css
-- import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
-- import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (..)
import Model.Locale exposing (ValueDetail(..))
import Msg.Graph exposing (Msg(..))
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..))
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import Theme.Html.Icons as Icons
import Theme.Html.SettingsPage as Sp
-- import Time
import Util.ThemedSelectBox as TSelectBox
import Util.ThemedSelectBoxes as TSelectBoxes
import Util.View
import View.Controls as Vc
import View.Locale as Locale
-- import View.User exposing (localeSwitch)


-- heading3 : List Css.Style
-- heading3 =
--     [ Css.fontWeight Css.bold
--     , Css.marginTop (Css.px 15)
--     , Css.px 10 |> Css.marginBottom
--     , Css.fontSize (Css.px 18)
--     , Css.px 10 |> Css.marginLeft
--     ]


-- heading4 : List Css.Style
-- heading4 =
--     [ Css.fontWeight Css.bold
--     , Css.marginTop (Css.px 15)
--     , Css.px 10 |> Css.paddingBottom
--     , Css.px 10 |> Css.paddingTop
--     ]


-- tableKey : List Css.Style
-- tableKey =
--     [ Css.minWidth (Css.px 500), Css.px 5 |> Css.paddingTop, Css.px 5 |> Css.paddingBottom, Css.px 10 |> Css.paddingLeft, Css.verticalAlign Css.middle ]


-- tableData : List Css.Style
-- tableData =
--     [ Css.verticalAlign Css.middle, Css.textAlign Css.right ]


-- tableStyle : List Css.Style
-- tableStyle =
--     [ Css.px 20 |> Css.marginLeft ]


-- type alias SelectOption =
--     { val : String
--     , selected : Bool
--     , lbl : String
--     }


-- type SettingsItem
--     = ToggleSwitch String Bool Model.Msg
--     | Display String String
--     | SubSection String
--     | Custom String (Html Model.Msg)
--     | Select String (String -> Model.Msg) (List SelectOption)


-- type Settings
--     = Section String (List SettingsItem)


-- currencyOptions : String -> List SelectOption
-- currencyOptions cs =
--     [ { val = "eur", selected = cs == "eur", lbl = "EUR" }
--     , { val = "usd", selected = cs == "usd", lbl = "USD" }
--     ]


-- addressLabelOptions : Graph.AddressLabelType -> List SelectOption
-- addressLabelOptions gc =
--     [ { val = "id", selected = gc == Graph.ID, lbl = "Id" }
--     , { val = "balance", selected = gc == Graph.Balance, lbl = "Balance" }
--     , { val = "total received", selected = gc == Graph.TotalReceived, lbl = "Total received" }
--     , { val = "tag", selected = gc == Graph.Tag, lbl = "Label" }
--     ]


-- valueFormatOptions : ValueDetail -> List SelectOption
-- valueFormatOptions vd =
--     [ { val = "exact", selected = vd == Exact, lbl = "Exact" }
--     , { val = "magnitude", selected = vd == Magnitude, lbl = "Magnitude" }
--     ]


-- transactionLableOptions : Graph.TxLabelType -> List SelectOption
-- transactionLableOptions lt =
--     [ { val = "notxs", selected = lt == Graph.NoTxs, lbl = "No. transactions" }
--     , { val = "value", selected = lt == Graph.Value, lbl = "Value" }
--     ]


view : Plugins -> Config -> Model x -> Html Model.Msg
view p vc m =
    let
        -- settings =
        --     [ Section "Profile" (authContent vc m.user)
        --     , Section "General"
        --         [ SubSection "General"
        --         , Custom "Language" (localeSwitch vc)
        --         , ToggleSwitch "Show date in user locale" vc.showDatesInUserLocale (UserClickedToggleDatesInUserLocale |> ChangedDisplaySettingsMsg |> PathfinderMsg)
        --         , Select "Preferred fiat currency" (UserChangedPreferredCurrency >> SettingsMsg) (currencyOptions vc.preferredFiatCurrency)
        --         , SubSection "Values"
        --         , ToggleSwitch "Show in fiat" vc.showValuesInFiat (UserToggledValueDisplay |> SettingsMsg)

        --         -- , Select "Change currency" (UserChangesCurrency >> GraphMsg) (currencyOptions vc.locale.currency)
        --         , Select "Value format" (UserChangesValueDetail >> GraphMsg) (valueFormatOptions vc.locale.valueDetail)
        --         ]
        --     , Section "Overview Network"
        --         [ SubSection "Transaction"
        --         , ToggleSwitch "Show zero value transactions" m.graph.config.showZeroTransactions (UserClickedToggleShowZeroTransactions |> GraphMsg)
        --         , Select "Transaction label" (UserChangesTxLabelType >> GraphMsg) (transactionLableOptions m.graph.config.txLabelType)
        --         , SubSection "Address"
        --         , Select "Address Label" (UserChangesAddressLabelType >> GraphMsg) (addressLabelOptions m.graph.config.addressLabelType)
        --         , SubSection "Show Shadow Links ..."
        --         , ToggleSwitch "for addresses" m.graph.config.showAddressShadowLinks (UserClickedShowAddressShadowLinks |> GraphMsg)
        --         , ToggleSwitch "for entities" m.graph.config.showEntityShadowLinks (UserClickedShowEntityShadowLinks |> GraphMsg)
        --         ]
        --     , Section "Pathfinder"
        --         [ SubSection "General"
        --         , ToggleSwitch "Snap to Grid" vc.snapToGrid (UserClickedToggleSnapToGrid |> ChangedDisplaySettingsMsg |> PathfinderMsg)
        --         , SubSection "Date"
        --         , ToggleSwitch "Show timezone" vc.showTimeZoneOffset (UserClickedToggleShowTimeZoneOffset |> ChangedDisplaySettingsMsg |> PathfinderMsg)
        --         , SubSection "Transaction"
        --         , ToggleSwitch "Show timestamp" vc.showTimestampOnTxEdge (UserClickedToggleShowTxTimestamp |> ChangedDisplaySettingsMsg |> PathfinderMsg)
        --         , SubSection "Cluster"
        --         , ToggleSwitch "Highlight on graph" vc.highlightClusterFriends (UserClickedToggleHighlightClusterFriends |> ChangedDisplaySettingsMsg |> PathfinderMsg)
        --         ]
        --     ]

        tbs =
            Vc.tabs
                [ { title = Locale.string vc.locale "General", selected = True, msg = Model.NoOp }
                , { title = Locale.string vc.locale "Pathfinder", selected = False, msg = Model.NoOp }
                , { title = Locale.string vc.locale "Overview Network", selected = False, msg = Model.NoOp }
                ]

        generalSettingsPageDummyData =
            { dropDownExtraTextClosed = { primaryText = "a", secondaryText = "b" }
            , languageDropDown = { text = "" }
            , settingsItemLabelOfSettingsCurrencyItem = { text = Locale.string vc.locale "Preferred fiat currency" }
            , settingsItemLabelOfSettingsLanguageItem = { text = Locale.string vc.locale "Language" }
            , settingsItemLabelOfSettingsModeItem = { text = Locale.string vc.locale "Mode" }
            , settingsItemLabelOfSettingsTimeZoneItem = { text = Locale.string vc.locale "Timezone" }
            , toggleCellNeutralOfToggleSwitchText = { toggleText = "" }
            , toggleCellSelectedOfToggleSwitchText = { toggleText = "" }
            }

        usdSelected =
            vc.preferredFiatCurrency == "usd"

        currencyToggle =
            Vc.toggleWithText
                { selectedA = usdSelected
                , titleA = "USD"
                , titleB = "EUR"
                , msg =
                    UserChangedPreferredCurrency
                        (if usdSelected then
                            "eur"

                         else
                            "usd"
                        )
                        |> SettingsMsg
                }

        modeToggle =
            Vc.toggleWithIcons
                { selectedA = vc.lightmode
                , iconA = Icons.iconsLightMode {}
                , iconB = Icons.iconsDarkMode {}
                , msg = Model.UserClickedLightmode
                }

        -- sb = TSelectBox.init (locales |> List.map (\ (a, b) -> {value = a, label = (Locale.string vc.locale b)}))
        sbId =
            TSelectBoxes.SupportedLanguages

        sb =
            TSelectBoxes.get sbId m.selectBoxes
                |> Maybe.withDefault TSelectBox.empty
                |> TSelectBox.mapLabel (Locale.string vc.locale)

        languageSb =
            TSelectBox.view sb vc.locale.locale |> Html.Styled.map (Model.SelectBoxMsg sbId)

        pageBody =
            Sp.settingsPageGeneralWithInstances
                Sp.settingsPageGeneralAttributes
                (Sp.settingsPageGeneralInstances
                    |> Rs.s_toggleSwitchText (Just currencyToggle)
                    |> Rs.s_toggleSwitchIcons (Just modeToggle)
                    |> Rs.s_languageDropDown (Just languageSb)
                )
                generalSettingsPageDummyData
    in
    div
        []
        [ Sp.settingsPageWithInstances
            Sp.settingsPageAttributes
            (Sp.settingsPageInstances
                |> Rs.s_settingsTabs (Just tbs)
            )
            { backButton = { buttonText = Locale.string vc.locale "Back", iconInstance = Icons.iconsArrowBack {} }
            , navbarPageTitle = { productLabel = Locale.string vc.locale "Settings" }
            , settingsPage = { instance = pageBody }
            , singleTab1 = { variant = Util.View.none }
            , singleTab2 = { variant = Util.View.none }
            }
        ]


-- viewSettings : Config -> Settings -> Html Model.Msg
-- viewSettings vc s =
--     case s of
--         Section label items ->
--             div []
--                 [ h4 [ heading3 |> css ] [ text (Locale.string vc.locale label) ]
--                 , table [ tableStyle |> css ] (items |> List.map (viewItem vc))
--                 ]


-- viewItem : Config -> SettingsItem -> Html Model.Msg
-- viewItem vc item =
--     tr []
--         (case item of
--             ToggleSwitch lbl stat msg ->
--                 [ td [ tableKey |> css ] [ text (Locale.string vc.locale lbl) ], td [ tableData |> css ] [ Util.View.onOffSwitch vc [ checked stat, onClick msg ] "" ] ]

--             Display lbl x ->
--                 [ td [ tableKey |> css ] [ text (Locale.string vc.locale lbl) ], td [ tableData |> css ] [ text x ] ]

--             SubSection x ->
--                 [ td [ heading4 |> css ] [ text (Locale.string vc.locale x) ], td [ tableData |> css ] [] ]

--             Custom lbl ctnt ->
--                 [ td [ tableKey |> css ] [ text (Locale.string vc.locale lbl) ], td [ tableData |> css ] [ ctnt ] ]

--             Select lbl msg options ->
--                 [ td [ tableKey |> css ] [ text (Locale.string vc.locale lbl) ]
--                 , td [ tableData |> css ]
--                     [ select
--                         [ Css.View.input vc |> css
--                         , onInput msg
--                         ]
--                         (options |> List.map (\x -> option [ value x.val, x.selected |> selected ] [ Locale.string vc.locale x.lbl |> text ]))
--                     ]
--                 ]
--         )


-- authContent : Config -> UserModel -> List SettingsItem
-- authContent vc user =
--     case user.auth of
--         Authorized auth ->
--             [ Display "Request limit" (auth.requestLimit |> requestLimit vc)
--             , Display "Expiration" (auth.expiration |> Maybe.map (expiration vc) |> Maybe.withDefault "none")
--             ]

--         Unknown ->
--             [ Locale.string vc.locale "Unknown" |> Display "Request Limit" ]

--         Unauthorized _ _ ->
--             [ Locale.string vc.locale "Please log-in" |> Display "Request Limit" ]


-- requestLimit : Config -> RequestLimit -> String
-- requestLimit vc rl =
--     case rl of
--         Unlimited ->
--             Locale.string vc.locale "unlimited"

--         Limited { remaining, limit, reset } ->
--             Locale.interpolated vc.locale "{0}/{1}" [ String.fromInt remaining, String.fromInt limit ]
--                 ++ (if reset == 0 || remaining > Model.showResetCounterAtRemaining then
--                         Locale.string vc.locale "None"

--                     else
--                         " "
--                             ++ (reset
--                                     |> String.fromInt
--                                     |> List.singleton
--                                     |> Locale.interpolated vc.locale "reset in {0}s"
--                                )
--                    )


-- expiration : Config -> Time.Posix -> String
-- expiration vc time =
--     Time.posixToMillis time
--         |> Locale.timestamp vc.locale
