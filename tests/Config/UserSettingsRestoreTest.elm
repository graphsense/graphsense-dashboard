module Config.UserSettingsRestoreTest exposing (suite)

{-| Every setting that gets saved has to be read back at boot.

`Model.userSettingsFromMainModel` decides what is persisted and
`Init.viewConfigFromSettings` decides what is restored. Nothing ties the two
together, so a field can be written to localStorage on every change and then
quietly dropped on the next start — which is exactly what `showBothValues` did:
"show fiat and crypto" saved correctly and came back off every time.

`Init.init` itself cannot be called from a test (its `Plugin.Model.Flags` argument
has a generated, per-plugin shape), which is why the config building is split out
into a function that can.

-}

import Config.UserSettings exposing (UserSettings)
import Config.View
import Dict
import Expect
import Init
import Support.Env as Env
import Test exposing (Test, describe, test)


{-| Settings with every restorable flag set _against_ its default, so a field that
is ignored at boot shows up as the default rather than the stored value.
-}
allFlipped : UserSettings
allFlipped =
    let
        base =
            Config.UserSettings.default "en"
    in
    { base
        | lightMode = Just False
        , showDatesInUserLocale = Just False
        , showTimeZoneOffset = Just True
        , showTimestampOnTxEdge = Just False
        , showValuesInFiat = Just True
        , preferredFiatCurrency = Just "eur"
        , showHash = Just True
        , showBothValues = Just True
    }


restored : Config.UserSettings.UserSettings -> Config.View.Config
restored settings =
    Init.viewConfigFromSettings False Env.viewConfig.locale Dict.empty settings


suite : Test
suite =
    describe "restoring the view config from saved settings"
        [ test "show fiat and crypto together" <|
            \_ ->
                (restored allFlipped).showBothValues
                    |> Expect.equal True
        , test "the other persisted flags come back too" <|
            \_ ->
                let
                    config =
                        restored allFlipped
                in
                Expect.equalLists
                    [ config.lightmode
                    , config.showDatesInUserLocale
                    , config.showTimeZoneOffset
                    , config.showTimestampOnTxEdge
                    , config.showValuesInFiat
                    , config.showHash
                    ]
                    [ False, False, True, False, True, True ]
        , test "the preferred fiat currency comes back" <|
            \_ ->
                (restored allFlipped).preferredFiatCurrency
                    |> Expect.equal "eur"
        , test "an empty settings record falls back to the defaults" <|
            \_ ->
                (restored (Config.UserSettings.default "en")).showBothValues
                    |> Expect.equal False
        ]
