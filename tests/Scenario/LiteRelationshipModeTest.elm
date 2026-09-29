module Scenario.LiteRelationshipModeTest exposing (suite)

{-| A network whose backend serves no relations takes no part in
relationship-based tracing: its nodes are drawn faded, the tooltip on such a
node says why, and the side panel offers no table at all instead of the
transactions table the user would mistake for counterparties.
-}

import Api.Data
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Html.Styled
import Json.Decode
import Model.NetworkCapabilities as NetworkCapabilities exposing (NetworkCapabilities)
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Util.Tooltip
import Util.TooltipType as TooltipType
import View.Locale as Locale


addressId : Id
addressId =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


btcWithoutRelations : NetworkCapabilities
btcWithoutRelations =
    NetworkCapabilities.fromApi { networks = [ { network = "btc", disabled = [ "relations" ] } ] }


withAddressFixture : (Api.Data.Address -> Expectation) -> Expectation
withAddressFixture f =
    case Json.Decode.decodeString Api.Data.addressDecoder Fixture.address of
        Ok address ->
            f address

        Err error ->
            Expect.fail ("the address fixture did not decode: " ++ Json.Decode.errorToString error)


answerAddress : Api.Data.Address -> App -> App
answerAddress address =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetAddressEffect _ toMsg ->
                    Just (toMsg address)

                _ ->
                    Nothing
        )


{-| The address opened from its deep link, selected, its side panel open.
-}
graphWithOneAddress : NetworkCapabilities -> Api.Data.Address -> App
graphWithOneAddress capabilities address =
    Route.addressRoute { network = "btc", address = Tuple.second addressId }
        |> App.initAt
        |> App.mapModel (\m -> { m | networkCapabilities = capabilities })
        |> answerAddress address


inRelationshipMode : App -> App
inRelationshipMode =
    App.step UserClickedToggleTracingMode


tooltipOfNode : App -> Query.Single Msg
tooltipOfNode app =
    Util.Tooltip.view Env.viewConfig (App.model app) (TooltipType.Address addressId)
        |> Html.Styled.div []
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


text : String -> Selector
text =
    Locale.string Env.viewConfig.locale >> Selector.text


tabLabel : String -> Selector
tabLabel =
    Locale.string Env.viewConfig.locale >> Locale.titleCase Env.viewConfig.locale >> Selector.text


whyFaded : Selector
whyFaded =
    text "Relationship mode not supported on lite networks"


suite : Test
suite =
    describe "relationship mode on a lite network"
        [ test "the side panel shows no table, not even the transactions" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        graphWithOneAddress btcWithoutRelations address
                            |> inRelationshipMode
                            |> App.html
                            |> Expect.all
                                [ Query.hasNot [ text "Transactions" ]
                                , Query.hasNot [ tabLabel "Outgoing relations" ]
                                , Query.hasNot [ tabLabel "Incoming relations" ]
                                ]
        , test "transaction mode keeps the transactions table on the same network" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        graphWithOneAddress btcWithoutRelations address
                            |> App.html
                            |> Query.has [ text "Transactions" ]
        , test "a network with relations keeps its relations tables" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        graphWithOneAddress NetworkCapabilities.none address
                            |> inRelationshipMode
                            |> App.html
                            |> Expect.all
                                [ Query.has [ tabLabel "Outgoing relations" ]
                                , Query.has [ tabLabel "Incoming relations" ]
                                ]
        , test "the tooltip of the faded node is only the hint, no figures" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        graphWithOneAddress btcWithoutRelations address
                            |> inRelationshipMode
                            |> tooltipOfNode
                            |> Expect.all
                                [ Query.has [ whyFaded ]
                                , Query.hasNot [ text "Balance" ]
                                ]
        , test "the tooltip shows the figures in transaction mode" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        graphWithOneAddress btcWithoutRelations address
                            |> tooltipOfNode
                            |> Query.has [ text "Balance" ]
        , test "the tooltip carries no such note in transaction mode" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        graphWithOneAddress btcWithoutRelations address
                            |> tooltipOfNode
                            |> Query.hasNot [ whyFaded ]
        , test "the tooltip carries no such note on a network with relations" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        graphWithOneAddress NetworkCapabilities.none address
                            |> inRelationshipMode
                            |> tooltipOfNode
                            |> Query.hasNot [ whyFaded ]
        ]
