module View.Settings exposing (view)

import Config.View exposing (Config)
import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (Auth(..), Model, Msg(..), RequestLimit(..), SettingsMsg(..), SettingsTabs(..), UserModel)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Theme.Html.Buttons as Btns
import Theme.Html.Icons as Icons
import Theme.Html.SettingsPage as Sp
import Time
import Util.ThemedSelectBox as TSelectBox
import Util.ThemedSelectBoxes as TSelectBoxes
import Util.View
import View.Controls as Vc
import View.Locale as Locale


view : Plugins -> Config -> Model x -> Html Model.Msg
view plugins vc m =
    -- let
    -- tbs =
    --     Vc.tabs
    --         ([ ( "General", GeneralTab )
    --          , ( "Pathfinder", PathfinderTab )
    --          , ( "Overview Network", GraphTab )
    --          ]
    --             |> List.map
    --                 (\( t, msg ) ->
    --                     { title = Locale.string vc.locale t
    --                     , selected = m.selectedSettingsTab == msg
    --                     , msg = Model.UserChangedSettingsTab msg |> Model.SettingsMsg
    --                     }
    --                 )
    --         )
    -- in
    Sp.settingsPageWithInstances
        (Sp.settingsPageAttributes
            |> Rs.s_backButton [ css [ Css.cursor Css.pointer ], onClick UserClickedNavBack ]
            |> Rs.s_settingsPage
                [ css [ Css.flexGrow <| Css.num 1 ]
                ]
        )
        (Sp.settingsPageInstances
            |> Rs.s_settingsTabs (Just Util.View.none)
        )
        { backButton = { buttonText = Locale.string vc.locale "Back", iconInstance = Icons.iconsArrowBack {} }
        , navbarPageTitle = { productLabel = Locale.string vc.locale "Settings" }
        , settingsPage =
            { instance =
                case m.selectedSettingsTab of
                    GeneralTab ->
                        generalSettings plugins vc m

                    GraphTab ->
                        graphSettings vc m

                    PathfinderTab ->
                        pathfinderSettings vc m
            }
        , singleTab1 = { variant = Util.View.none }
        , singleTab2 = { variant = Util.View.none }
        }


pathfinderSettings : Config -> Model x -> Html Model.Msg
pathfinderSettings _ _ =
    Util.View.none


graphSettings : Config -> Model x -> Html Model.Msg
graphSettings _ _ =
    Util.View.none


generalSettings : Plugins -> Config -> Model x -> Html Model.Msg
generalSettings plugins vc m =
    let
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
            Vc.lightModeToggle
                { selectedA = vc.lightmode
                , msg = Model.UserClickedLightmode
                }

        sbId =
            TSelectBoxes.SupportedLanguages

        sb =
            TSelectBoxes.get sbId m.selectBoxes
                |> Maybe.withDefault TSelectBox.empty
                |> TSelectBox.mapLabel (Locale.string vc.locale)

        languageSb =
            TSelectBox.view sb vc.locale.locale |> Html.Styled.map (Model.SelectBoxMsg sbId)

        ( expr, ( rqlPrim, rqlSec ) ) =
            authContent vc m.user

        generalSettingsProperties =
            { button =
                { variant =
                    Btns.buttonTypeTextStateRegularStyleTextWithAttributes
                        (Btns.buttonTypeTextStateRegularStyleTextAttributes |> Rs.s_button [ [ Css.cursor Css.pointer ] |> css, onClick UserClickedLogout ])
                        { typeTextStateRegularStyleText = { buttonText = Locale.string vc.locale "Logout", iconInstance = Util.View.none, iconVisible = False } }
                }
            , dropDownExtraTextClosed = { primaryText = "a", secondaryText = "b" }
            , languageDropDown = { text = "" }
            , leftCell = { variant = Util.View.none }
            , modeToggle = { variant = modeToggle }
            , rightCell = { variant = Util.View.none }
            , settingsItemLabelOfSettingsCurrencyItem = { text = Locale.string vc.locale "Preferred fiat currency" }
            , settingsItemLabelOfSettingsLanguageItem = { text = Locale.string vc.locale "Language" }
            , settingsItemLabelOfSettingsModeItem = { text = Locale.string vc.locale "Mode" }
            , settingsItemLabelOfSettingsTimeZoneItem = { text = Locale.string vc.locale "Timezone" }
            , settingsSectionHeaderOfPlanDetails = { text = Locale.string vc.locale "Plan Details" }
            , settingsExpirationRow3 =
                { secondaryTextVisible = False
                , secondaryValueText = ""
                , titleText = Locale.string vc.locale "Expires on"
                , valueText = expr
                }
            , settingsUsageRow4 =
                { secondaryTextVisible = True
                , secondaryValueText = rqlSec |> Maybe.map (\x -> "/" ++ x) |> Maybe.withDefault ""
                , titleText = Locale.string vc.locale "Usage Limit"
                , valueText = rqlPrim
                }
            }

        pluginProfiles =
            Plugin.profile plugins m.plugins vc
    in
    Sp.settingsPageGeneralWithInstances
        (Sp.settingsPageGeneralAttributes
            |> Rs.s_pluginSettingsList
                [ css
                    (if List.isEmpty pluginProfiles then
                        [ Css.display Css.none
                        ]

                     else
                        [ Css.height Css.auto
                        ]
                    )
                ]
            |> Rs.s_planDetails [ css [ Css.height Css.auto ] ]
            |> Rs.s_settingsPageGeneral [ css [ Css.height Css.auto ] ]
        )
        (Sp.settingsPageGeneralInstances
            |> Rs.s_toggleSwitchText (Just currencyToggle)
            |> Rs.s_languageDropDown (Just languageSb)
            |> Rs.s_settingsTimeZoneItem (Just Util.View.none)
        )
        { pluginSettingsList =
            pluginProfiles
                |> List.map
                    (\( title, part ) ->
                        div
                            [ css Sp.settingsPageGeneralPlanDetails_details.styles
                            , css [ Css.height Css.auto |> Css.important ]
                            ]
                            [ Sp.settingsSectionHeader
                                { settingsSectionHeader = { text = title } }
                            , part
                            ]
                    )
        }
        generalSettingsProperties


authContent : Config -> UserModel -> ( String, ( String, Maybe String ) )
authContent vc user =
    case user.auth of
        Authorized auth ->
            ( auth.expiration |> Maybe.map (expiration vc) |> Maybe.withDefault (Locale.string vc.locale "Never")
            , auth.requestLimit |> requestLimit vc
            )

        Unknown ->
            ( Locale.string vc.locale "Unknown", ( "", Nothing ) )

        Unauthorized _ _ ->
            ( Locale.string vc.locale "Please log-in", ( "", Nothing ) )


requestLimit : Config -> RequestLimit -> ( String, Maybe String )
requestLimit vc rl =
    case rl of
        Unlimited ->
            ( Locale.string vc.locale "unlimited", Nothing )

        Limited { remaining, limit, reset } ->
            ( String.fromInt remaining
            , Just
                (String.fromInt limit
                    ++ " "
                    ++ (if reset == 0 || remaining > Model.showResetCounterAtRemaining then
                            Locale.string vc.locale "None"

                        else
                            reset
                                |> String.fromInt
                                |> List.singleton
                                |> Locale.interpolated vc.locale "reset in {0}s"
                       )
                )
            )


expiration : Config -> Time.Posix -> String
expiration vc time =
    Time.posixToMillis time
        |> Locale.timestamp vc.locale
