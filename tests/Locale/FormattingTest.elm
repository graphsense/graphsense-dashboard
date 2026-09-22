module Locale.FormattingTest exposing (suite)

{-| Value formatting is what the user reads off every node, table row and
detail panel. It is pure, locale-dependent and full of thresholds (the number
of decimals a coin gets depends on its magnitude), so it is both easy to get
wrong and easy to test.

The models are built through the real `Init.Locale.init` path, so the tests
exercise the same locale-specific `numberFormat` the app installs.

-}

import Api.Data
import Config.UserSettings
import Dict exposing (Dict)
import Expect
import Init.Locale
import Model.Currency exposing (Currency(..))
import Model.Locale exposing (Model, State(..), ValueDetail(..))
import RecordSetter exposing (s_mapping, s_supportedTokens, s_valueDetail)
import Test exposing (Test, describe, test)
import Time
import View.Locale as Locale



-- MODELS


{-| `Init.Locale.init` leaves `mapping` empty (translations are fetched over
HTTP), which is fine: `View.Locale.string` then falls back to the key, and only
the date/time formats below need a real entry.
-}
localeModel : String -> Model
localeModel locale =
    Config.UserSettings.default locale
        |> Init.Locale.init
        |> Tuple.first
        |> s_valueDetail Exact
        |> s_supportedTokens supportedTokens


magnitude : Model -> Model
magnitude =
    s_valueDetail Magnitude


en : Model
en =
    localeModel "en"


de : Model
de =
    localeModel "de"


{-| USDT with 6 decimals on ethereum, so the token path is covered next to the
built-in fixpoint factors (eth 1e18, btc 1e8).
-}
supportedTokens : Dict String Api.Data.TokenConfigs
supportedTokens =
    Dict.fromList
        [ ( "eth"
          , { tokenConfigs =
                [ { contractAddress = Just "0xdac17f958d2ee523a2206206994597c13d831ec7"
                  , decimals = 6
                  , pegCurrency = Just "usd"
                  , ticker = "usdt"
                  }
                ]
            }
          )

        -- display metadata a served swap leg carried, registered the way
        -- Update.RegisterConversionAsset does: two registrations spell the same
        -- ticker with different decimals — the contract address, not the
        -- ticker, is the key
        , ( "bnb"
          , { tokenConfigs =
                [ { contractAddress = Just usdtLikeContract
                  , decimals = 9
                  , pegCurrency = Just "market"
                  , ticker = "USDT"
                  }
                , { contractAddress = Just akeContract
                  , decimals = 18
                  , pegCurrency = Just "market"
                  , ticker = "AKE"
                  }
                , { contractAddress = Just "0x55d398326f99059ff775485246999027b3197955"
                  , decimals = 18
                  , pegCurrency = Just "usd"
                  , ticker = "usdt"
                  }
                ]
            }
          )
        ]


usdtLikeContract : String
usdtLikeContract =
    "0x1111111111111111111111111111111111111111"


akeContract : String
akeContract =
    "0x2c3a8ee94ddd97244a93bc48298f97d2c412f7db"


asset : String -> String -> Model.Currency.AssetIdentifier
asset network ticker =
    { network = network, asset = ticker }


values : Int -> List ( String, Float ) -> Api.Data.Values
values value fiats =
    { value = value
    , fiatValues = fiats |> List.map (\( code, v ) -> { code = code, value = v })
    }



-- TESTS


suite : Test
suite =
    describe "View.Locale formatting"
        [ describe "coin values"
            [ table "btc, exact"
                (\( v, expected ) -> Locale.coin en (asset "btc" "btc") v |> Expect.equal expected)
                [ ( 0, "0 BTC" )
                , ( 100000000, "1.00 BTC" )
                , ( 250000000, "2.50 BTC" )

                -- below 1 the number of decimals follows the magnitude:
                -- two significant digits, so smaller values get more decimals
                , ( 10000000, "0.100 BTC" )
                , ( 1000, "0.0000100 BTC" )
                , ( 1, "0.0000000100 BTC" )
                ]
            , test "large btc amounts keep their thousands separators" <|
                \_ ->
                    Locale.coin en (asset "btc" "btc") 123456700000000
                        |> Expect.equal "1,234,567.00 BTC"
            , test "eth uses the 1e18 fixpoint factor" <|
                \_ ->
                    Locale.coin en (asset "eth" "eth") 1500000000000000000
                        |> Expect.equal "1.50 ETH"
            , test "a token uses the decimals from its config" <|
                \_ ->
                    -- usdt has 6 decimals, not eth's 18
                    Locale.coin en (asset "eth" "usdt") 2500000
                        |> Expect.equal "2.50 USDT"
            , test "an unknown asset says so instead of showing a wrong number" <|
                \_ ->
                    Locale.coin en (asset "eth" "nosuchtoken") 2500000
                        |> Expect.equal "unknown currency nosuchtoken"
            , test "a transfer keyed by contract address reads by its registered symbol" <|
                \_ ->
                    -- a swap leg keyed by contract address: the conversion
                    -- carried the curated symbol and decimals
                    -- (computed: an integer literal this large would overflow
                    -- the compiler's 64-bit parse; the app gets it as a JSON double)
                    Locale.coin (magnitude en) (asset "bnb" akeContract) (34154 * 10 ^ 18)
                        |> Expect.equal "34.15k AKE"
            , test "the contract address is matched case-insensitively" <|
                \_ ->
                    Locale.coin (magnitude en) (asset "bnb" (String.toUpper akeContract |> String.replace "0X" "0x")) (10 ^ 18)
                        |> Expect.equal "1.00 AKE"
            , test "a contract-keyed asset is scaled by the decimals registered for that address" <|
                \_ ->
                    -- 9 decimals, not the 18 of the curated usdt whose ticker it spells
                    Locale.coin en (asset "bnb" usdtLikeContract) 5000000000
                        |> Expect.equal "5.00 USDT"
            , test "a ticker-keyed asset still scales by the registry's decimals" <|
                \_ ->
                    Locale.coin en (asset "bnb" "usdt") (5 * 10 ^ 18)
                        |> Expect.equal "5.00 USDT"
            , test "coinWithoutCode drops the ticker" <|
                \_ ->
                    Locale.coinWithoutCode en (asset "btc" "btc") 100000000
                        |> Expect.equal "1.00"
            , test "german uses a comma as the decimal separator" <|
                \_ ->
                    Locale.coin de (asset "btc" "btc") 123456700000000
                        |> Expect.equal "1.234.567,00 BTC"
            ]
        , describe "fiat display"
            [ test "an asset without a fiat quote falls back to its coin amount" <|
                \_ ->
                    -- a leg the backend could not price carries no quote: the
                    -- coin amount, never a fabricated "0.00 USD"
                    Locale.currency (Fiat "usd") en [ ( asset "bnb" akeContract, values (10 ^ 18) [] ) ]
                        |> Expect.equal "1.00 AKE"
            , test "a priced asset shows its fiat value" <|
                \_ ->
                    Locale.currency (Fiat "usd") en [ ( asset "eth" "usdt", values 2500000 [ ( "usd", 2.5 ) ] ) ]
                        |> Expect.equal "2.50 USD"
            , test "hasFiat tells the two apart" <|
                \_ ->
                    ( Locale.hasFiat [ ( asset "bnb" akeContract, values 1 [] ) ]
                    , Locale.hasFiat [ ( asset "eth" "usdt", values 1 [ ( "usd", 0 ) ] ) ]
                    )
                        |> Expect.equal ( False, True )
            ]
        , describe "value detail"
            [ test "exact spells the number out" <|
                \_ ->
                    Locale.coin en (asset "btc" "btc") 1234500000000
                        |> Expect.equal "12,345.00 BTC"
            , test "magnitude abbreviates it" <|
                \_ ->
                    Locale.coin (magnitude en) (asset "btc" "btc") 1234500000000
                        |> Expect.equal "12.35k BTC"
            , test "magnitude abbreviates fiat too" <|
                \_ ->
                    Locale.fiat (magnitude en) "usd" 1234567.0
                        |> Expect.equal "1.23m USD"
            ]
        , describe "fiat values"
            [ table "usd, exact"
                (\( v, expected ) -> Locale.fiat en "usd" v |> Expect.equal expected)
                [ ( 0, "0.00 USD" )
                , ( 1, "1.00 USD" )
                , ( 1234.5, "1,234.50 USD" )
                , ( -1234.5, "-1,234.50 USD" )
                ]
            , test "fiatWithoutCode drops the code" <|
                \_ -> Locale.fiatWithoutCode en "usd" 1234.5 |> Expect.equal "1,234.50"
            , test "the code is upper-cased" <|
                \_ -> Locale.fiat en "eur" 1.0 |> Expect.equal "1.00 EUR"
            ]
        , describe "currency selection"
            [ test "Coin picks the asset and appends the number of other assets" <|
                \_ ->
                    -- with no fiat values to rank by, the raw integer values are
                    -- compared: eth's 1e18 base units beat usdt's 1e6, even
                    -- though both are worth "1.00" of their own unit
                    Locale.currency Coin
                        en
                        [ ( asset "eth" "eth", values 1000000000000000000 [] )
                        , ( asset "eth" "usdt", values 1000000 [] )
                        ]
                        |> Expect.equal "1.00 ETH +1"
            , test "Coin on a single asset appends nothing" <|
                \_ ->
                    Locale.currency Coin
                        en
                        [ ( asset "eth" "eth", values 1000000000000000000 [] ) ]
                        |> Expect.equal "1.00 ETH"
            , test "Fiat sums the values of every asset" <|
                \_ ->
                    Locale.currency (Fiat "usd")
                        en
                        [ ( asset "eth" "eth", values 0 [ ( "usd", 10.5 ) ] )
                        , ( asset "eth" "usdt", values 0 [ ( "usd", 4.5 ) ] )
                        ]
                        |> Expect.equal "15.00 USD"
            , test "Fiat ignores assets quoted in another fiat currency" <|
                \_ ->
                    Locale.currency (Fiat "usd")
                        en
                        [ ( asset "eth" "eth", values 0 [ ( "eur", 10.5 ) ] ) ]
                        |> Expect.equal "0.00 USD"
            , test "all-zero multi-asset values collapse to a plain zero" <|
                \_ ->
                    Locale.currency Coin
                        en
                        [ ( asset "eth" "eth", values 0 [] )
                        , ( asset "eth" "usdt", values 0 [] )
                        ]
                        |> Expect.equal "0"
            ]
        , describe "integers and percentages"
            [ test "int groups thousands" <|
                \_ -> Locale.int en 1234567 |> Expect.equal "1,234,567"
            , test "int groups thousands the german way" <|
                \_ -> Locale.int de 1234567 |> Expect.equal "1.234.567"
            , test "int abbreviates in magnitude mode" <|
                \_ -> Locale.int (magnitude en) 1234567 |> Expect.equal "1m"
            , test "intWithoutValueDetailFormatting ignores the value detail" <|
                \_ ->
                    Locale.intWithoutValueDetailFormatting (magnitude en) 1234567
                        |> Expect.equal "1,234,567"
            , table "percentage"
                (\( v, expected ) -> Locale.percentage en v |> Expect.equal expected)
                [ ( 0, "0%" )
                , ( 0.5, "50%" )
                , ( 0.1234, "12.34%" )
                , ( 1, "100%" )
                ]
            ]
        , describe "timestamps"
            [ test "the uniform date format does not depend on the translations" <|
                \_ ->
                    Locale.timestampDateUniform en (Time.millisToPosix 1700000000000)
                        |> Expect.equal "Nov 14, 2023"
            , test "the uniform time format is 24h" <|
                \_ ->
                    Locale.timestampTimeUniform en False (Time.millisToPosix 1700000000000)
                        |> Expect.equal "22:13:20 "
            , test "month names follow the locale" <|
                \_ ->
                    -- December is one of the months whose abbreviation differs
                    ( Locale.timestampDateUniform en (Time.millisToPosix 1703462400000)
                    , Locale.timestampDateUniform de (Time.millisToPosix 1703462400000)
                    )
                        |> Expect.equal ( "Dec 25, 2023", "Dez 25, 2023" )
            , test "the translated date format is used when one is loaded" <|
                \_ ->
                    en
                        |> s_mapping (Settled (Dict.fromList [ ( "date-format", "yyyy-MM-dd" ) ]))
                        |> (\model -> Locale.date model (Time.millisToPosix 1700000000000))
                        |> Expect.equal "2023-11-14"
            ]
        ]


{-| One `test` per row, named after the input so a failure points at the case.
-}
table : String -> (( a, String ) -> Expect.Expectation) -> List ( a, String ) -> Test
table name run rows =
    rows
        |> List.map (\row -> test (name ++ ": " ++ Tuple.second row) (\_ -> run row))
        |> describe name
