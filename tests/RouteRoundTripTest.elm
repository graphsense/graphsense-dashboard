module RouteRoundTripTest exposing (suite)

{-| `Route.toUrl` and `Route.parse` are the two halves of every deep link,
every plugin hand-off and every `Nav.pushUrl` in the app. They are written
independently of each other, so nothing but a test keeps them in sync.

The fuzzers deliberately draw identifiers from a URL-safe alphabet, which is
what real addresses, transaction hashes and actor ids look like. Neither side
escapes path segments — `Url.Builder.absolute` joins them raw and
`Util.Url.Parser.preparePath` splits them raw — so free-text identifiers
containing `/` or `?` do not survive. The "known boundaries" tests at the
bottom pin that down rather than pretend it works.

-}

import Expect
import Fuzz exposing (Fuzzer)
import Model.DateFilter as DateFilter
import Route exposing (Route)
import Route.Pathfinder as Pathfinder
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Time
import Url



-- CONFIG


config : Route.Config
config =
    { pathfinder = { networks = networks }
    }


networks : List String
networks =
    [ "btc", "eth", "trx" ]



-- FUZZERS


network : Fuzzer String
network =
    Fuzz.oneOfValues networks


{-| Base58/hex-ish: the character set actual cryptoasset identifiers use.
-}
identifier : Fuzzer String
identifier =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        |> String.toList
        |> Fuzz.oneOfValues
        |> Fuzz.listOfLengthBetween 1 40
        |> Fuzz.map String.fromList


{-| Whole milliseconds between 1970 and 2100 — `Iso8601` is exact there.
-}
posix : Fuzzer Time.Posix
posix =
    Fuzz.intRange 0 4102444800000
        |> Fuzz.map Time.millisToPosix


dateFilter : Fuzzer DateFilter.DateFilterRaw
dateFilter =
    Fuzz.map2 DateFilter.init (Fuzz.maybe posix) (Fuzz.maybe posix)


thing : Fuzzer Pathfinder.Thing
thing =
    Fuzz.oneOf
        [ Fuzz.map2 Pathfinder.Address identifier (Fuzz.maybe dateFilter)
        , Fuzz.map Pathfinder.Tx identifier
        , Fuzz.map Pathfinder.Block (Fuzz.intRange 0 1000000000)
        , Fuzz.map2 Pathfinder.Relation identifier identifier
        ]


hop : Fuzzer Pathfinder.PathHopType
hop =
    Fuzz.oneOf
        [ Fuzz.map2 Pathfinder.AddressHop
            (Fuzz.oneOfValues
                [ Pathfinder.VictimAddress
                , Pathfinder.PerpetratorAddress
                , Pathfinder.NormalAddress
                ]
            )
            identifier
        , Fuzz.map Pathfinder.TxHop identifier
        ]


{-| `Pathfinder.Plugin` is left out: its `PluginType` comes from the generated
plugin code, which is empty in the default (plugin-less) test configuration.
-}
pathfinderRoute : Fuzzer Pathfinder.Route
pathfinderRoute =
    Fuzz.oneOf
        [ Fuzz.constant Pathfinder.Root
        , Fuzz.map Pathfinder.Actor identifier
        , Fuzz.map Pathfinder.Label identifier
        , Fuzz.map2 Pathfinder.Network network thing

        -- an empty hop list has no URL representation, see "known boundaries"
        , Fuzz.map2 Pathfinder.Path network (Fuzz.listOfLengthBetween 1 5 hop)
        ]


route : Fuzzer Route
route =
    Fuzz.oneOf
        [ Fuzz.map Route.Pathfinder pathfinderRoute
        , Fuzz.constant Route.Home
        , Fuzz.constant Route.Stats
        , Fuzz.constant Route.Settings
        , Fuzz.constant Route.RetiredGraph
        ]



-- HELPERS


parse : String -> Maybe Route
parse path =
    ("https://example.com" ++ path)
        |> Url.fromString
        |> Maybe.andThen (Route.parse config)


roundTrips : Route -> Expect.Expectation
roundTrips r =
    Route.toUrl r
        |> parse
        |> Expect.equal (Just r)



-- TESTS


suite : Test
suite =
    describe "Route round trip"
        [ fuzz route "toUrl >> parse is the identity" roundTrips
        , fuzz pathfinderRoute "every pathfinder route survives toUrl >> parse" <|
            \r -> roundTrips (Route.Pathfinder r)
        , fuzz2 network identifier "an address route keeps its network and address" <|
            \net address ->
                Pathfinder.addressRoute { network = net, address = address }
                    |> Route.Pathfinder
                    |> roundTrips
        , fuzz2 network identifier "a tx route keeps its network and hash" <|
            \net hash ->
                Pathfinder.txRoute { network = net, txHash = hash }
                    |> Route.Pathfinder
                    |> roundTrips
        , describe "known boundaries"
            [ test "an unconfigured network does not parse" <|
                \_ ->
                    parse "/pathfinder/doge/address/DH5yaieqoZN36fDVciNyRueRGvGLR3mr7L"
                        |> Expect.equal Nothing
            , test "an empty path has no URL representation" <|
                \_ ->
                    -- Path with no hops renders as "/pathfinder/btc/path/", whose
                    -- trailing empty segment is dropped before parsing. Callers
                    -- must not build one.
                    Pathfinder.pathRoute "btc" []
                        |> Route.Pathfinder
                        |> Route.toUrl
                        |> parse
                        |> Expect.equal Nothing
            , test "a space in an identifier is fine" <|
                \_ ->
                    Pathfinder.Label "foo bar"
                        |> Route.Pathfinder
                        |> roundTrips
            , test "a slash in an identifier loses the route" <|
                \_ ->
                    -- Neither side escapes path segments: Url.Builder.absolute
                    -- joins them raw and Util.Url.Parser.preparePath splits them
                    -- raw. Addresses and tx hashes are alphanumeric so this never
                    -- bites there, but labels and actor ids are free text.
                    Pathfinder.Label "foo/bar"
                        |> Route.Pathfinder
                        |> Route.toUrl
                        |> parse
                        |> Expect.equal Nothing
            , test "a question mark in an identifier truncates it" <|
                \_ ->
                    -- everything after it is read as the query string
                    Pathfinder.Label "foo?bar"
                        |> Route.Pathfinder
                        |> Route.toUrl
                        |> parse
                        |> Expect.equal (Just (Route.Pathfinder (Pathfinder.Label "foo")))
            ]
        ]
