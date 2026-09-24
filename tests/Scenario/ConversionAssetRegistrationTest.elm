module Scenario.ConversionAssetRegistrationTest exposing (suite)

{-| A served dex swap names its legs' assets, and the Pathfinder hands that
curated symbol/decimals pair up to the shell as a token config so the value
formatter can label and scale the leg (`RegisterConversionAsset`).

That registration races the per-network token list: the two write the same
`supportedTokens` entry, and the swap answer can arrive first — a deep link
waits for the capabilities, not for the token list. These pin that neither
write loses: a registered asset survives the list arriving, and a network that
only ever got a registration still asks for its list.

The merged entry is the formatter's. The Stats page's "Supported tokens" pills
show only the network's list as it arrived, so a curated leg the list lacks is
labelled in the graph but never advertised as a supported token.

-}

import Api.Data
import Dict exposing (Dict)
import Effect.Api
import Expect
import Http
import Init.Pathfinder.Id as Id
import Model exposing (Effect(..), Msg(..), listedTokens)
import Model.Pathfinder.Network exposing (FindPosition(..))
import Msg.Pathfinder as Pathfinder
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)



-- FIXTURE: one curated dex swap inside one bnb tx


hash : String
hash =
    "63336a5ace33dc969cdb769f64b8499eae7f142741895fa4d589dbfa41bf5d95"


swapper : String
swapper =
    "0xe17ad4f88be9cd479a8036a058b893a0da62bc7a"


settlement : String
settlement =
    "0x9008d19f58aabd9ed0d60971565aa8510560ab41"


inputLegId : String
inputLegId =
    hash ++ "_T95"


outputLegId : String
outputLegId =
    hash ++ "_T108"


usd1Contract : String
usd1Contract =
    "0x8d0d000ee44948fc98c9b98a4fa4921476f08b0d"


inputLegTx : Api.Data.Tx
inputLegTx =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = "bnb"
        , fee = Nothing
        , fromAddress = swapper
        , height = 119317568
        , identifier = inputLegId
        , isExternal = Nothing
        , network = "bnb"
        , timestamp = 1788254088
        , toAddress = settlement
        , tokenTxId = Nothing
        , txHash = hash
        , txType = "account"
        , value = { fiatValues = [], value = 1 }
        }


{-| A dex swap whose token leg carries the curated display metadata the
adapter serves; `contract` is the casing that leg's asset arrives in.
-}
curatedSwap : String -> Api.Data.ExternalConversion
curatedSwap contract =
    { conversionType = Api.Data.ExternalConversionConversionTypeDexSwap
    , fromAddress = swapper
    , fromAmount = "0x16a4ecb955b8a31b"
    , fromAsset = "native"
    , fromAssetTransfer = "0x" ++ inputLegId
    , fromIsSupportedAsset = True
    , fromNetwork = "bnb"
    , toAddress = swapper
    , toAmount = "0x16a56e085c4dad4f"
    , toAsset = contract
    , toAssetTransfer = "0x" ++ outputLegId
    , toIsSupportedAsset = True
    , toNetwork = "bnb"
    , fromAssetSymbol = Nothing
    , toAssetSymbol = Just "USD1"
    , fromAssetDecimals = Nothing
    , toAssetDecimals = Just 18
    , fromAmountFiatValues = Nothing
    , toAmountFiatValues = Nothing
    }


{-| `/supported_tokens` for the same network: the dashboard's own list, which
does not carry the swapped token.
-}
tokenList : Api.Data.TokenConfigs
tokenList =
    { tokenConfigs =
        [ { contractAddress = Just "0x55d398326f99059ff775485246999027b3197955"
          , decimals = 18
          , pegCurrency = Just "USD"
          , ticker = "USDT"
          }
        ]
    }


{-| The same list, but it does carry the swapped token — under its own ticker
and in the checksummed casing the API serves.
-}
tokenListWithUsd1 : Api.Data.TokenConfigs
tokenListWithUsd1 =
    { tokenConfigs =
        { contractAddress = Just "0x8D0D000Ee44948FC98c9B98A4FA4921476f08B0d"
        , decimals = 18
        , pegCurrency = Just "USD"
        , ticker = "WORLD LIBERTY USD"
        }
            :: tokenList.tokenConfigs
    }


stats : Api.Data.Stats
stats =
    statsFor [ "bnb" ]


statsFor : List String -> Api.Data.Stats
statsFor networks =
    { currencies =
        networks
            |> List.map
                (\network ->
                    { name = network
                    , noAddressRelations = 1
                    , noAddresses = 1
                    , noBlocks = 1
                    , noEntities = 1
                    , noLabels = 1
                    , noTaggedAddresses = 1
                    , noTxs = 1
                    , timestamp = 1788254088
                    }
                )
    , requestTimestamp = "2026-09-22T00:00:00"
    , version = "1.0.0"
    }



-- DRIVING


{-| Puts the swap's input leg on the graph, the way an answered tx request does.
-}
withInputLeg : App -> App
withInputLeg =
    App.step
        (PathfinderMsg
            (Pathfinder.BrowserGotTx
                { pos = Auto
                , loadAddresses = False
                , autoLinkInTraceMode = False
                , requestedTxHash = inputLegId
                }
                inputLegTx
            )
        )


{-| The API answered the leg's `/conversions` with the curated swap, which is
what makes the Pathfinder register the token leg's asset.
-}
gotCuratedSwap : String -> App -> App
gotCuratedSwap contract app =
    Dict.get (Id.init "bnb" inputLegId) (App.model app).pathfinder.network.txs
        |> Maybe.map (\tx -> App.step (PathfinderMsg (Pathfinder.BrowserGotConversions tx [ curatedSwap contract ])) app)
        |> Maybe.withDefault app


register : App -> App
register app =
    app |> withInputLeg |> gotCuratedSwap usd1Contract


gotTokenList : Api.Data.TokenConfigs -> App -> App
gotTokenList configs =
    App.step (BrowserGotSupportedTokens "bnb" configs)


gotStatistics : App -> App
gotStatistics =
    App.step (BrowserGotStatistics stats)


{-| The API answered bnb's `/supported_tokens` with a server error.
-}
tokenListFailed : App -> App
tokenListFailed =
    App.step
        (BrowserGotResponseWithHeaders Nothing
            (Err
                ( Http.BadStatus 500
                , Dict.empty
                , Effect.Api.ListSupportedTokensEffect "bnb" (BrowserGotSupportedTokens "bnb")
                )
            )
        )


asksForTokenList : Effect -> Bool
asksForTokenList =
    asksForTokenListOf "bnb"


asksForTokenListOf : String -> Effect -> Bool
asksForTokenListOf network eff =
    case eff of
        ApiEffect (Effect.Api.ListSupportedTokensEffect currency _) ->
            currency == network

        _ ->
            False


{-| How many token-list requests the last step made, per network.
-}
tokenListRequests : App -> ( Int, Int )
tokenListRequests app =
    ( App.effects app |> List.filter (asksForTokenListOf "bnb") |> List.length
    , App.effects app |> List.filter (asksForTokenListOf "eth") |> List.length
    )


{-| What the model knows about bnb's tokens, as ( contract address, ticker ).
-}
knownTokens : App -> List ( String, String )
knownTokens app =
    App.model app
        |> .supportedTokens
        |> tokensOf


{-| The same, from what the Stats page is handed: the network's own list.
-}
listed : App -> List ( String, String )
listed app =
    App.model app
        |> listedTokens
        |> tokensOf


{-| The same, from the copy the value formatter reads.
-}
formatterTokens : App -> List ( String, String )
formatterTokens app =
    App.model app
        |> .config
        |> .locale
        |> .supportedTokens
        |> tokensOf


tokensOf : Dict String Api.Data.TokenConfigs -> List ( String, String )
tokensOf tokens =
    Dict.get "bnb" tokens
        |> Maybe.map .tokenConfigs
        |> Maybe.withDefault []
        |> List.map (\c -> ( c.contractAddress |> Maybe.withDefault "" |> String.toLower, c.ticker ))
        |> List.sort


usdt : ( String, String )
usdt =
    ( "0x55d398326f99059ff775485246999027b3197955", "USDT" )



-- SCENARIOS


suite : Test
suite =
    describe "a conversion asset registration and the token list share an entry"
        [ describe "a registered swap asset"
            [ test "lands in the model" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> knownTokens
                        |> Expect.equal [ ( usd1Contract, "USD1" ) ]
            , test "lands in the copy the value formatter reads" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> formatterTokens
                        |> Expect.equal [ ( usd1Contract, "USD1" ) ]
            , test "is registered once, whatever casing the asset arrives in" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotCuratedSwap (String.toUpper usd1Contract)
                        |> knownTokens
                        |> Expect.equal [ ( usd1Contract, "USD1" ) ]
            ]
        , describe "when the token list arrives after a registration"
            [ test "the registered asset survives in the model" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotTokenList tokenList
                        |> knownTokens
                        |> Expect.equal [ usdt, ( usd1Contract, "USD1" ) ]
            , test "and in the copy the value formatter reads" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotTokenList tokenList
                        |> formatterTokens
                        |> Expect.equal [ usdt, ( usd1Contract, "USD1" ) ]
            , test "a token the list carries itself is taken from the list" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotTokenList tokenListWithUsd1
                        |> knownTokens
                        |> Expect.equal [ usdt, ( usd1Contract, "WORLD LIBERTY USD" ) ]
            ]
        , describe "the Stats page"
            [ test "lists only what the network's token list carries" <|
                \_ ->
                    App.initAt "/"
                        |> gotTokenList tokenList
                        |> register
                        |> (\app -> ( listed app, knownTokens app, formatterTokens app ))
                        |> Expect.equal
                            ( [ usdt ]
                            , [ usdt, ( usd1Contract, "USD1" ) ]
                            , [ usdt, ( usd1Contract, "USD1" ) ]
                            )
            , test "a registration alone creates no Stats pills" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> App.model
                        |> listedTokens
                        |> Dict.member "bnb"
                        |> Expect.equal False
            ]
        , describe "when the statistics arrive after a registration"
            [ test "the network's token list is still asked for" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotStatistics
                        |> App.expectEffect "ListSupportedTokensEffect bnb" asksForTokenList
            , test "a network whose list already arrived is not asked again" <|
                \_ ->
                    App.initAt "/"
                        |> gotTokenList tokenList
                        |> gotStatistics
                        |> App.expectNoEffect "ListSupportedTokensEffect bnb" asksForTokenList
            ]
        , describe "when a token list request fails"
            [ test "a failed token list is requested again on the next statistics answer" <|
                \_ ->
                    App.initAt "/"
                        |> App.step (BrowserGotStatistics (statsFor [ "bnb", "eth" ]))
                        |> App.step (BrowserGotSupportedTokens "eth" tokenList)
                        |> tokenListFailed
                        |> App.step (BrowserGotStatistics (statsFor [ "bnb", "eth" ]))
                        |> tokenListRequests
                        |> Expect.equal ( 1, 0 )
            ]
        ]
