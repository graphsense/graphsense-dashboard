module Scenario.GovListTagTest exposing (suite)

{-| Addresses on a governmental black or white list get a black or white tag
icon instead of the usual one.
-}

import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Html
import Html.Attributes
import Json.Decode
import Model.Pathfinder.Id exposing (Id)
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Util.Pathfinder.GovList as GovList exposing (GovList(..))


addressId : Id
addressId =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


{-| A label carrying one concept, either the address's own or inherited.
-}
type alias Label =
    ( String, Maybe Api.Data.LabelSummaryInheritedFrom )


direct : String -> Label
direct concept =
    ( concept, Nothing )


fromCluster : String -> Label
fromCluster concept =
    ( concept, Just Api.Data.LabelSummaryInheritedFromCluster )


fromPubkey : String -> Label
fromPubkey concept =
    ( concept, Just Api.Data.LabelSummaryInheritedFromPubkey )


{-| The spec's tag summary, with one label per entry of `labels`. The concept
cloud lists every concept regardless of origin, as the API's does.
-}
withSummary : List Label -> (Api.Data.TagSummary -> Expectation) -> Expectation
withSummary labels f =
    case Json.Decode.decodeString Api.Data.tagSummaryDecoder Fixture.tagSummary of
        Ok summary ->
            let
                template =
                    Dict.values summary.labelSummary |> List.head
            in
            case template of
                Just label ->
                    f
                        { summary
                            | conceptTagCloud =
                                labels
                                    |> List.map (\( c, _ ) -> ( c, { cnt = 1, weighted = 1 } ))
                                    |> Dict.fromList
                            , labelSummary =
                                labels
                                    |> List.indexedMap
                                        (\i ( c, inheritedFrom ) ->
                                            ( String.fromInt i
                                            , { label
                                                | label = String.fromInt i
                                                , concepts = [ c ]
                                                , inheritedFrom = inheritedFrom
                                              }
                                            )
                                        )
                                    |> Dict.fromList
                        }

                Nothing ->
                    Expect.fail "the tag summary fixture has no label"

        Err error ->
            Expect.fail ("the tag summary fixture did not decode: " ++ Json.Decode.errorToString error)


{-| Puts the fixture address on the graph and answers its tag summary request,
whichever of the single or bulk lookups the node uses.
-}
graphWithTaggedAddress : Api.Data.TagSummary -> Api.Data.Address -> App
graphWithTaggedAddress summary address =
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


withAddress : (Api.Data.Address -> Expectation) -> Expectation
withAddress f =
    case Json.Decode.decodeString Api.Data.addressDecoder Fixture.address of
        Ok address ->
            f address

        Err error ->
            Expect.fail ("the address fixture did not decode: " ++ Json.Decode.errorToString error)


nodeGovList : App -> Maybe (Maybe GovList)
nodeGovList =
    App.model >> .network >> .addresses >> Dict.get addressId >> Maybe.map .govList


nodeShows : List Label -> List String -> Expectation
nodeShows labels testIds =
    withSummary labels <|
        \summary ->
            withAddress <|
                \address ->
                    graphWithTaggedAddress summary address
                        |> App.html
                        |> Query.find [ Selector.attribute (dataTestId "gs-address-node") ]
                        |> Expect.all
                            (List.map
                                (\( name, expected ) query ->
                                    query
                                        |> Query.findAll [ Selector.attribute (dataTestId name) ]
                                        |> Query.count (Expect.equal expected)
                                )
                                (List.map (\n -> ( n, boolToInt (List.member n testIds) ))
                                    [ "gs-gov-blacklist-tag", "gs-gov-whitelist-tag" ]
                                )
                            )


dataTestId : String -> Html.Attribute Never
dataTestId =
    Html.Attributes.attribute "data-testid"


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0


suite : Test
suite =
    describe "governmental list tags"
        [ describe "GovList.fromTagSummary"
            [ test "recognises a governmental blacklist" <|
                \_ ->
                    withSummary [ direct "sanction", direct "gov_black_list" ] (GovList.fromTagSummary >> Expect.equal (Just GovBlackList))
            , test "recognises a governmental whitelist" <|
                \_ ->
                    withSummary [ direct "gov_white_list" ] (GovList.fromTagSummary >> Expect.equal (Just GovWhiteList))
            , test "a blacklist wins over a whitelist" <|
                \_ ->
                    withSummary [ direct "gov_white_list", direct "gov_black_list" ] (GovList.fromTagSummary >> Expect.equal (Just GovBlackList))
            , test "non-governmental lists are ordinary tags" <|
                \_ ->
                    withSummary [ direct "black_list", direct "white_list" ] (GovList.fromTagSummary >> Expect.equal Nothing)
            , test "ignores a listing inherited from the cluster" <|
                \_ ->
                    withSummary [ direct "organization", fromCluster "gov_black_list" ] (GovList.fromTagSummary >> Expect.equal Nothing)
            , test "ignores a listing inherited from a shared pubkey" <|
                \_ ->
                    withSummary [ fromPubkey "gov_white_list" ] (GovList.fromTagSummary >> Expect.equal Nothing)
            , test "an inherited blacklist does not beat a direct whitelist" <|
                \_ ->
                    withSummary [ direct "gov_white_list", fromCluster "gov_black_list" ] (GovList.fromTagSummary >> Expect.equal (Just GovWhiteList))
            ]
        , describe "the graph node"
            [ test "remembers the list from the tag summary" <|
                \_ ->
                    withSummary [ direct "gov_black_list" ] <|
                        \summary ->
                            withAddress
                                (graphWithTaggedAddress summary
                                    >> nodeGovList
                                    >> Expect.equal (Just (Just GovBlackList))
                                )
            , test "draws a blacklist tag" <|
                \_ -> nodeShows [ direct "gov_black_list" ] [ "gs-gov-blacklist-tag" ]
            , test "draws a whitelist tag" <|
                \_ -> nodeShows [ direct "gov_white_list" ] [ "gs-gov-whitelist-tag" ]
            , test "draws an ordinary tag otherwise" <|
                \_ -> nodeShows [ direct "organization" ] []
            , test "draws an ordinary tag when only the cluster is listed" <|
                \_ -> nodeShows [ direct "organization", fromCluster "gov_black_list" ] []
            ]
        ]
