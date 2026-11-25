module Scenario.AddressPanelTagIconTest exposing (suite)

{-| The side panel's tag icon, in front of the categories row. An address
whose tags are all exchange tags has no tag icon on the graph, where the node
itself is drawn as an exchange; the panel still shows one
(0x4b5c…34f6 on eth, tagged Coinbase, had none).
-}

import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Html.Attributes
import Json.Decode
import Model.Pathfinder.Id exposing (Id)
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


addressId : Id
addressId =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


{-| The spec's tag summary with one own label carrying `concept`, and
`tagCount` tags of the address's own.
-}
summaryWith : String -> Int -> Maybe Api.Data.TagSummary
summaryWith concept tagCount =
    Json.Decode.decodeString Api.Data.tagSummaryDecoder Fixture.tagSummary
        |> Result.toMaybe
        |> Maybe.andThen
            (\summary ->
                Dict.values summary.labelSummary
                    |> List.head
                    |> Maybe.map
                        (\label ->
                            { summary
                                | tagCount = tagCount
                                , broadCategory = concept
                                , conceptTagCloud = Dict.singleton concept { cnt = 1, weighted = 1 }
                                , labelSummary =
                                    Dict.singleton "Coinbase"
                                        { label | label = "Coinbase", concepts = [ concept ], inheritedFrom = Nothing }
                            }
                        )
            )


opened : Api.Data.TagSummary -> Maybe App
opened summary =
    Json.Decode.decodeString Api.Data.addressDecoder Fixture.address
        |> Result.toMaybe
        |> Maybe.map
            (\address ->
                Route.addressRoute { network = "btc", address = Tuple.second addressId }
                    |> App.initAt
                    |> App.respond
                        (\eff ->
                            case eff of
                                Effect.Api.GetAddressEffect _ toMsg ->
                                    Just (toMsg address)

                                _ ->
                                    Nothing
                        )
                    |> App.respond
                        (\eff ->
                            case eff of
                                Effect.Api.GetAddressTagSummaryEffect _ toMsg ->
                                    Just (toMsg summary)

                                Effect.Api.BulkGetAddressTagSummaryEffect _ toMsg ->
                                    Just (toMsg [ ( addressId, summary ) ])

                                _ ->
                                    Nothing
                        )
            )


{-| How many direct tag icons the side panel shows.
-}
directTagIcons : String -> Int -> Expectation
directTagIcons concept count =
    case summaryWith concept 1 |> Maybe.andThen opened of
        Just app ->
            Expect.all
                [ \a -> Dict.member addressId (App.model a).network.addresses |> Expect.equal True
                , App.html
                    >> Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-direct-tag") ]
                    >> Query.count (Expect.equal count)
                ]
                app

        Nothing ->
            Expect.fail "the fixtures did not decode"


suite : Test
suite =
    describe "the side panel's tag icon"
        [ test "an address tagged only as an exchange gets it" <|
            \_ -> directTagIcons "exchange" 1
        , test "an address with another tag gets it" <|
            \_ -> directTagIcons "organization" 1
        ]
