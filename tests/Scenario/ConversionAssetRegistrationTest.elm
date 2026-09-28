module Scenario.ConversionAssetRegistrationTest exposing (suite)

{-| A served dex swap's curated leg asset (`RegisterConversionAsset`) and the
network's `/supported_tokens` list arrive in either order. The formatter reads
both; asset filters, ticker scaling and the Stats pills only the list.
-}

import Api.Data
import Dict
import Effect.Api
import Expect
import Http
import Init.Pathfinder.Id as Id
import Model exposing (Effect(..), Msg(..))
import Model.Locale
import Msg.Pathfinder as Pathfinder
import Support.MainApp as App exposing (App)
import Support.SwapFixture exposing (accountTx, dexSwap, inputLegId, settlement, swapper, txRequest, usd1Contract)
import Test exposing (Test, describe, test)
import View.Locale as Locale



-- FIXTURE: one curated dex swap inside one bnb tx


{-| USD1's contract in the EIP-55 checksummed casing the API serves.
-}
usd1Checksummed : String
usd1Checksummed =
    "0x8D0D000Ee44948FC98c9B98A4FA4921476f08B0d"


inputLegTx : Api.Data.Tx
inputLegTx =
    accountTx inputLegId swapper settlement


{-| A dex swap whose token leg carries the curated display metadata the
adapter serves; `contract` is the casing that leg's asset arrives in.
-}
curatedSwap : String -> Api.Data.ExternalConversion
curatedSwap contract =
    { dexSwap
        | fromAsset = "native"
        , toAsset = contract
        , toAssetSymbol = Just "USD1"
        , toAssetDecimals = Just 18
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


{-| The same list, but it does carry the swapped token, under its own ticker.
-}
tokenListWithUsd1 : Api.Data.TokenConfigs
tokenListWithUsd1 =
    { tokenConfigs =
        { contractAddress = Just usd1Checksummed
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
    App.step (PathfinderMsg (Pathfinder.BrowserGotTx (txRequest inputLegId) inputLegTx))


{-| The API answered the leg's `/conversions` with the curated swap, which is
what makes the Pathfinder register the token leg's asset.
-}
gotCuratedSwap : String -> App -> App
gotCuratedSwap =
    curatedSwap >> gotSwap


gotSwap : Api.Data.ExternalConversion -> App -> App
gotSwap swap app =
    Dict.get (Id.init "bnb" inputLegId) (App.model app).pathfinder.network.txs
        |> Maybe.map (\tx -> App.step (PathfinderMsg (Pathfinder.BrowserGotConversions tx [ swap ])) app)
        |> Maybe.withDefault app


register : App -> App
register app =
    app |> withInputLeg |> gotCuratedSwap usd1Contract


{-| A curated leg on another contract whose symbol spells the listed `USDT`
ticker, with other decimals.
-}
registerUsdtNamesake : App -> App
registerUsdtNamesake =
    let
        swap =
            curatedSwap "0x1111111111111111111111111111111111111111"
    in
    withInputLeg >> gotSwap { swap | toAssetSymbol = Just "USDT", toAssetDecimals = Just 9 }


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


{-| bnb's curated swap assets, as ( contract address, ticker ).
-}
registered : App -> List ( String, String )
registered app =
    (App.model app).config.locale.swapAssets
        |> Dict.get "bnb"
        |> Maybe.withDefault []
        |> tokensOf


{-| bnb's token list as the Stats page is handed it.
-}
listed : App -> List ( String, String )
listed app =
    (App.model app).supportedTokens
        |> Dict.get "bnb"
        |> Maybe.map .tokenConfigs
        |> Maybe.withDefault []
        |> tokensOf


tokensOf : List Api.Data.TokenConfig -> List ( String, String )
tokensOf =
    List.map (\c -> ( c.contractAddress |> Maybe.withDefault "" |> String.toLower, c.ticker ))
        >> List.sort


{-| The tickers the tx and address asset filters offer for bnb.
-}
filterTickers : App -> List String
filterTickers app =
    Model.Locale.getTokenTickers (App.model app).config.locale "bnb"


{-| How the value formatter reads a bnb amount of `asset`.
-}
formatted : String -> Int -> App -> String
formatted asset value app =
    Locale.coin (App.model app).config.locale { network = "bnb", asset = asset } value


usdt : ( String, String )
usdt =
    ( "0x55d398326f99059ff775485246999027b3197955", "USDT" )



-- SCENARIOS


suite : Test
suite =
    describe "a conversion asset registration and the token list"
        [ describe "a registered swap asset"
            [ test "lands in the swap assets" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> registered
                        |> Expect.equal [ ( usd1Contract, "USD1" ) ]
            , test "labels and scales the leg in the value formatter" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> formatted usd1Contract (10 ^ 18)
                        |> Expect.equal "1.00 USD1"
            , test "is registered once, whatever casing the asset arrives in" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotCuratedSwap usd1Checksummed
                        |> registered
                        |> Expect.equal [ ( usd1Contract, "USD1" ) ]
            ]
        , describe "when the token list arrives after a registration"
            [ test "the registered asset survives" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotTokenList tokenList
                        |> registered
                        |> Expect.equal [ ( usd1Contract, "USD1" ) ]
            , test "and the formatter still reads the leg by it" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotTokenList tokenList
                        |> formatted usd1Contract (10 ^ 18)
                        |> Expect.equal "1.00 USD1"
            , test "a token the list carries itself is taken from the list" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> gotTokenList tokenListWithUsd1
                        |> formatted usd1Contract (10 ^ 18)
                        |> Expect.equal "1.00 WORLD LIBERTY USD"
            ]
        , describe "the Stats page"
            [ test "lists only what the network's token list carries" <|
                \_ ->
                    App.initAt "/"
                        |> gotTokenList tokenList
                        |> register
                        |> (\app -> ( listed app, registered app, formatted usd1Contract (10 ^ 18) app ))
                        |> Expect.equal
                            ( [ usdt ]
                            , [ ( usd1Contract, "USD1" ) ]
                            , "1.00 USD1"
                            )
            , test "a registration alone creates no Stats pills" <|
                \_ ->
                    App.initAt "/"
                        |> register
                        |> (\app -> ( registered app, Dict.member "bnb" (App.model app).supportedTokens ))
                        |> Expect.equal ( [ ( usd1Contract, "USD1" ) ], False )
            ]
        , describe "the asset filters"
            [ test "offer only the network's list, whichever arrives first" <|
                \_ ->
                    ( App.initAt "/" |> gotTokenList tokenList |> register |> filterTickers
                    , App.initAt "/" |> register |> gotTokenList tokenList |> filterTickers
                    )
                        |> Expect.equal ( [ "USDT" ], [ "USDT" ] )
            ]
        , describe "a curated symbol that spells a listed ticker"
            [ test "does not rescale the listed ticker, whichever arrives first" <|
                \_ ->
                    ( App.initAt "/" |> gotTokenList tokenList |> registerUsdtNamesake |> formatted "usdt" (5 * 10 ^ 18)
                    , App.initAt "/" |> registerUsdtNamesake |> gotTokenList tokenList |> formatted "usdt" (5 * 10 ^ 18)
                    )
                        |> Expect.equal ( "5.00 USDT", "5.00 USDT" )
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
