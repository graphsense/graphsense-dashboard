module Config.TestUserSettings exposing (suite)

import Config.UserSettings as UserSettings exposing (UserSettings)
import Expect
import Json.Decode
import Json.Encode
import Model.Search exposing (ResultLine(..))
import Test exposing (Test, describe, test)


defaults : UserSettings
defaults =
    UserSettings.default "en"


withRecents : List ResultLine -> UserSettings
withRecents recents =
    { defaults | recentSearches = recents }


roundtrip : UserSettings -> Result Json.Decode.Error UserSettings
roundtrip settings =
    UserSettings.encoder settings
        |> Json.Decode.decodeValue UserSettings.decoder


{-| Simulates the real port → localStorage → reload path:
Elm Value → JSON.stringify (for storage) → JSON.parse (via flag decode) → Elm decoder.
-}
stringRoundtrip : UserSettings -> Result Json.Decode.Error UserSettings
stringRoundtrip settings =
    UserSettings.encoder settings
        |> Json.Encode.encode 0
        |> Json.Decode.decodeString UserSettings.decoder


suite : Test
suite =
    describe "UserSettings roundtrip"
        [ test "empty recents survive roundtrip" <|
            \_ ->
                withRecents []
                    |> roundtrip
                    |> Result.map .recentSearches
                    |> Expect.equal (Ok [])
        , test "single Address recent survives roundtrip" <|
            \_ ->
                let
                    recents =
                        [ Address "btc" "1abc" ]
                in
                withRecents recents
                    |> roundtrip
                    |> Result.map .recentSearches
                    |> Expect.equal (Ok recents)
        , test "mixed ResultLine variants survive roundtrip" <|
            \_ ->
                let
                    recents =
                        [ Address "btc" "1abc"
                        , Tx "eth" "0xdeadbeef"
                        , Block "btc" 700000
                        , Label "exchange"
                        , Actor ( "actor-id", "Actor Label" )
                        , Custom { id = "c1", label = "Custom Thing" }
                        ]
                in
                withRecents recents
                    |> roundtrip
                    |> Result.map .recentSearches
                    |> Expect.equal (Ok recents)
        , test "recents survive JSON-string roundtrip (simulates port path)" <|
            \_ ->
                let
                    recents =
                        [ Address "btc" "1abc"
                        , Tx "eth" "0xdeadbeef"
                        , Label "exchange"
                        ]
                in
                withRecents recents
                    |> stringRoundtrip
                    |> Result.map .recentSearches
                    |> Expect.equal (Ok recents)
        , test "corrupt recentSearches field does not break the whole decoder" <|
            \_ ->
                let
                    corruptPayload =
                        """{"selectedLanguage":"en","recentSearches":"not valid JSON at all"}"""
                in
                Json.Decode.decodeString UserSettings.decoder corruptPayload
                    |> Result.map .selectedLanguage
                    |> Expect.equal (Ok "en")
        ]
