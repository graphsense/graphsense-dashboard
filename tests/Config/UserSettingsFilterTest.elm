module Config.UserSettingsFilterTest exposing (suite)

import Config.UserSettings as UserSettings exposing (UserSettings)
import Dict
import Expect
import Init.Pathfinder
import Json.Decode
import Json.Encode
import Model.Pathfinder.Overview as Overview
import Test exposing (Test, describe, test)


{-| Save and reload as the browser does it: `main.js` hands each field to
`localStorage.setItem`, which keeps strings and stringifies everything else,
and reads them back as strings.
-}
roundTrip : UserSettings -> Maybe UserSettings
roundTrip =
    UserSettings.encoder
        >> Json.Decode.decodeValue (Json.Decode.dict Json.Decode.value)
        >> Result.map
            (Dict.map
                (\_ v ->
                    Json.Decode.decodeValue Json.Decode.string v
                        |> Result.withDefault (Json.Encode.encode 0 v)
                        |> Json.Encode.string
                )
                >> Dict.toList
                >> Json.Encode.object
            )
        >> Result.andThen (Json.Decode.decodeValue UserSettings.decoder)
        >> Result.toMaybe


suite : Test
suite =
    describe "On-graph filter preferences in the user settings"
        [ test "the limit and the hidden overview survive save and reload" <|
            \_ ->
                let
                    base =
                        UserSettings.default "en"
                in
                roundTrip { base | graphFilterLimit = Just 250, overviewHidden = Just True }
                    |> Maybe.map (\s -> ( s.graphFilterLimit, s.overviewHidden ))
                    |> Expect.equal (Just ( Just 250, Just True ))
        , test "settings saved before the filters existed still load" <|
            \_ ->
                Json.Decode.decodeString UserSettings.decoder """{"selectedLanguage":"en","snapToGrid":"true"}"""
                    |> Result.map (\s -> ( s.graphFilterLimit, s.overviewHidden ))
                    |> Expect.equal (Ok ( Nothing, Nothing ))
        , test "the Pathfinder starts with the saved limit and a hidden overview" <|
            \_ ->
                Init.Pathfinder.init
                    { snapToGrid = Nothing
                    , highlightClusterFriends = Nothing
                    , tracingMode = Nothing
                    , avoidOverlapingNodes = Nothing
                    , graphFilterLimit = Just 250
                    , overviewHidden = Just True
                    , recentSearches = []
                    }
                    |> Tuple.first
                    |> (\m -> ( m.graphFilter.limit, m.overview.visibility ))
                    |> Expect.equal ( 250, Overview.Hidden )
        ]
