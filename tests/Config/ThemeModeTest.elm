module Config.ThemeModeTest exposing (suite)

{-| The System / Light / Dark setting, and the one-time move from the old
boolean `lightMode`.

The inputs mirror what `main.js` hands over: `{...localStorage}`, one string per
key, so a bool arrives as `"true"` / `"false"`.

-}

import Config.UserSettings as UserSettings
import Config.View
import Dict
import Expect
import Init
import Json.Decode
import Model.ThemeMode exposing (ThemeMode(..))
import Support.Env as Env
import Test exposing (Test, describe, test)


storedMode : String -> Maybe ThemeMode
storedMode json =
    Json.Decode.decodeString UserSettings.decoder json
        |> Result.toMaybe
        |> Maybe.andThen .themeMode


restoredWith : Bool -> String -> Config.View.Config
restoredWith prefersDark json =
    Json.Decode.decodeString UserSettings.decoder json
        |> Result.withDefault (UserSettings.default "en")
        |> Init.viewConfigFromSettings False prefersDark Env.viewConfig.locale Dict.empty


suite : Test
suite =
    describe "theme mode"
        [ describe "migrating the old lightMode"
            [ test "lightMode true was the default, not a choice: follow the system" <|
                \_ ->
                    storedMode """{"selectedLanguage":"en","lightMode":"true"}"""
                        |> Expect.equal Nothing
            , test "lightMode false was an explicit choice of dark" <|
                \_ ->
                    storedMode """{"selectedLanguage":"en","lightMode":"false"}"""
                        |> Expect.equal (Just Dark)
            , test "nothing stored follows the system" <|
                \_ ->
                    storedMode """{"selectedLanguage":"en"}"""
                        |> Expect.equal Nothing
            , test "a stored themeMode wins over the old key" <|
                \_ ->
                    storedMode """{"selectedLanguage":"en","lightMode":"false","themeMode":"light"}"""
                        |> Expect.equal (Just Light)
            , test "an unknown themeMode falls back to the old key" <|
                \_ ->
                    storedMode """{"selectedLanguage":"en","lightMode":"false","themeMode":"sepia"}"""
                        |> Expect.equal (Just Dark)
            ]
        , test "every mode survives save and reload" <|
            \_ ->
                [ System, Light, Dark ]
                    |> List.map
                        (\mode ->
                            let
                                base =
                                    UserSettings.default "en"
                            in
                            UserSettings.encoder { base | themeMode = Just mode }
                                |> Json.Decode.decodeValue UserSettings.decoder
                                |> Result.toMaybe
                                |> Maybe.andThen .themeMode
                        )
                    |> Expect.equalLists [ Just System, Just Light, Just Dark ]
        , describe "resolving at boot"
            [ test "System follows a dark system" <|
                \_ ->
                    (restoredWith True """{"selectedLanguage":"en"}""").lightmode
                        |> Expect.equal False
            , test "System follows a light system" <|
                \_ ->
                    (restoredWith False """{"selectedLanguage":"en"}""").lightmode
                        |> Expect.equal True
            , test "Light ignores a dark system" <|
                \_ ->
                    (restoredWith True """{"selectedLanguage":"en","themeMode":"light"}""").lightmode
                        |> Expect.equal True
            , test "Dark ignores a light system" <|
                \_ ->
                    (restoredWith False """{"selectedLanguage":"en","themeMode":"dark"}""").lightmode
                        |> Expect.equal False
            ]
        ]
