module NetworkCapabilitiesTest exposing (suite)

import Api.Data
import Expect
import Model.NetworkCapabilities as NC
import Test exposing (Test, describe, test)


currencyStats : String -> Maybe (List String) -> Api.Data.CurrencyStats
currencyStats name capabilities =
    { name = name
    , noAddressRelations = 0
    , noAddresses = 0
    , noBlocks = 0
    , noEntities = 0
    , noLabels = 0
    , noTaggedAddresses = 0
    , noTxs = 0
    , timestamp = 0
    , coinTicker = Nothing
    , coinDecimals = Nothing
    , networkName = Nothing
    , capabilities = capabilities
    }


stats : List Api.Data.CurrencyStats -> Api.Data.Stats
stats currencies =
    { currencies = currencies
    , requestTimestamp = ""
    , version = ""
    }


suite : Test
suite =
    describe "NetworkCapabilities"
        [ test "absent capabilities field = full core network" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withStats (stats [ currencyStats "eth" Nothing ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "eth" |> Expect.equal False
                        , \caps -> NC.supports NC.Relations caps "eth" |> Expect.equal True
                        , \caps -> NC.supports NC.Tags caps "eth" |> Expect.equal True
                        ]
        , test "empty capabilities list = lite network supporting nothing" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withStats (stats [ currencyStats "arb" (Just []) ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "arb" |> Expect.equal True
                        , \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Clusters caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal False
                        ]
        , test "declared feature is supported, others are not, network stays lite" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withStats (stats [ currencyStats "bnb" (Just [ "tags" ]) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Tags caps "bnb" |> Expect.equal True
                        , \caps -> NC.supports NC.Relations caps "bnb" |> Expect.equal False
                        , \caps -> NC.isLimitedNetwork caps "bnb" |> Expect.equal True
                        ]
        , test "unknown vocabulary words are ignored" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withStats (stats [ currencyStats "arb" (Just [ "teleportation", "tags" ]) ])
                    |> (\caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True)
        , test "matching is case-insensitive on network and capability" <|
            \_ ->
                NC.fromBuildConfig [ " BNB " |> String.trim ]
                    |> NC.withStats (stats [ currencyStats "ARB" (Just [ "Tags" ]) ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "bnb" |> Expect.equal True
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True
                        ]
        , test "build-config seed marks a network lite before stats arrive" <|
            \_ ->
                NC.fromBuildConfig [ "bnb", "arb" ]
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "bnb" |> Expect.equal True
                        , \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        , \caps -> NC.isLimitedNetwork caps "eth" |> Expect.equal False
                        ]
        , test "a silent stats entry keeps the build-config seed" <|
            \_ ->
                NC.fromBuildConfig [ "bnb" ]
                    |> NC.withStats (stats [ currencyStats "bnb" Nothing ])
                    |> (\caps -> NC.isLimitedNetwork caps "bnb" |> Expect.equal True)
        , test "a declaring stats entry overrides the build-config seed" <|
            \_ ->
                NC.fromBuildConfig [ "bnb" ]
                    |> NC.withStats (stats [ currencyStats "bnb" (Just [ "tags" ]) ])
                    |> (\caps -> NC.supports NC.Tags caps "bnb" |> Expect.equal True)
        ]
