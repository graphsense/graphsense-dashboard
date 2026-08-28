module RouteTest exposing (suite)

import Expect
import Route exposing (Route)
import Test exposing (Test, describe, test)
import Url


config : Route.Config
config =
    { pathfinder = { networks = [ "btc", "eth" ] }
    }


parse : String -> Maybe Route
parse path =
    ("https://example.com" ++ path)
        |> Url.fromString
        |> Maybe.andThen (Route.parse config)


suite : Test
suite =
    describe "Route.parse"
        [ test "legacy /graph lands on the retired page" <|
            \_ ->
                parse "/graph"
                    |> Expect.equal (Just Route.RetiredGraph)
        , test "legacy /graph sub-urls land on the retired page" <|
            \_ ->
                parse "/graph/btc/address/1Archive1n2C579dMsAu3iC6tWzuQJz8dN"
                    |> Expect.equal (Just Route.RetiredGraph)
        , test "legacy /graph urls with query land on the retired page" <|
            \_ ->
                parse "/graph/btc/address/xyz?foo=bar"
                    |> Expect.equal (Just Route.RetiredGraph)
        , test "root is home" <|
            \_ ->
                parse "/"
                    |> Expect.equal (Just Route.Home)
        , test "stats still parses" <|
            \_ ->
                parse "/stats"
                    |> Expect.equal (Just Route.Stats)
        , test "retired page url roundtrips" <|
            \_ ->
                Route.toUrl Route.RetiredGraph
                    |> Expect.equal "/graph"
        ]
