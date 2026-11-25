module Scenario.AddressUsageRowsTest exposing (suite)

{-| An address the backend knows but that has no transactions of its own
names no `first_tx` / `last_tx`: a lite network answers 200 like that for an
EVM address used only on other chains (0x4d5d…f540 on bnb, arb, gnosis, base
and pol). The side panel then leaves out the First usage and Last usage rows
instead of showing their titles with no value.
-}

import Api.Data
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Msg.Pathfinder
import Route.Pathfinder as Route
import Support.App as App
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


{-| The address opened from its deep link and answered with `address`.
-}
panel : String -> String -> (Api.Data.Address -> Api.Data.Address) -> Query.Single Msg.Pathfinder.Msg
panel network addr change =
    Route.addressRoute { network = network, address = addr }
        |> App.initAt
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.GetAddressEffect _ toMsg ->
                        Json.Decode.decodeString Api.Data.addressDecoder Fixture.address
                            |> Result.toMaybe
                            |> Maybe.map (\a -> toMsg (change { a | currency = network, address = addr }))

                    _ ->
                        Nothing
            )
        |> App.html


unused : Api.Data.Address -> Api.Data.Address
unused a =
    { a | firstTx = Nothing, lastTx = Nothing, noIncomingTxs = 0, noOutgoingTxs = 0 }


expectRows : { first : Bool, last : Bool } -> Query.Single msg -> Expectation
expectRows { first, last } html =
    let
        row present title =
            if present then
                Query.has [ Selector.text title ]

            else
                Query.hasNot [ Selector.text title ]
    in
    html
        |> Expect.all
            [ row first "First usage"
            , row last "Last usage"
            ]


evmAddress : String
evmAddress =
    "0x4d5d0da4848cf76ca3f916c7c8b2c772cd47f540"


suite : Test
suite =
    describe "the usage rows of the address panel"
        [ describe "an account address (eth)"
            [ test "with first and last tx shows both rows" <|
                \_ ->
                    panel "eth" evmAddress identity
                        |> expectRows { first = True, last = True }
            , test "without first and last tx shows neither row" <|
                \_ ->
                    panel "eth" evmAddress unused
                        |> expectRows { first = False, last = False }
            , test "with only a first tx shows only its row" <|
                \_ ->
                    panel "eth" evmAddress (\a -> { a | lastTx = Nothing })
                        |> expectRows { first = True, last = False }
            ]
        , describe "a UTXO address (btc)"
            [ test "with first and last tx shows both rows" <|
                \_ ->
                    panel "btc" "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" identity
                        |> expectRows { first = True, last = True }
            , test "without first and last tx shows neither row" <|
                \_ ->
                    panel "btc" "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" unused
                        |> expectRows { first = False, last = False }
            ]
        ]
