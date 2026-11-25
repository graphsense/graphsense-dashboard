module SearchActorsTest exposing (suite)

{-| The main search boxes (top bar, Pathfinder) find actors too: they ask the
API for them and list them with the addresses and transactions. Picking one
leads to the actor route, which opens the actor dialog (see
Scenario.ActorDialogTest).
-}

import Api.Data
import Autocomplete
import Effect.Search
import Expect
import Html.Styled
import Init.Search
import Model.Search
import Msg.Search
import RecordSetter as Rs
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Update.Search
import View.Search


apiAnswer : Api.Data.SearchResult
apiAnswer =
    { currencies = []
    , labels = []
    , actors = Just [ { id = "binance", label = "Binance" } ]
    }


searchBox : Model.Search.Model
searchBox =
    Init.Search.init (Init.Search.initSearchAddressAndTxs Nothing)
        |> Rs.s_visible True


{-| How many actor lines a search box offers for an answer with eight actors.
-}
actorsListed : Model.Search.Model -> Int
actorsListed box =
    let
        manyActors =
            { apiAnswer | actors = Just (List.map (\i -> { id = "actor" ++ String.fromInt i, label = "Actor " ++ String.fromInt i }) (List.range 1 8)) }
    in
    box
        |> (\search -> Rs.s_autocomplete (search.autocomplete |> Autocomplete.setQuery "actor" |> Autocomplete.setStatus Autocomplete.Fetching) search)
        |> Update.Search.update (Msg.Search.BrowserGotSearchResult "actor" manyActors)
        |> Tuple.first
        |> .autocomplete
        |> Autocomplete.choices
        |> List.filter
            (\line ->
                case line of
                    Model.Search.Actor _ ->
                        True

                    _ ->
                        False
            )
        |> List.length


suite : Test
suite =
    describe "Actors in the main search box"
        [ test "the request asks for actors" <|
            \_ ->
                searchBox
                    |> Rs.s_autocomplete (searchBox.autocomplete |> Autocomplete.setQuery "binance")
                    |> Update.Search.maybeTriggerSearch
                    |> List.filterMap
                        (\eff ->
                            case eff of
                                Effect.Search.SearchEffect e ->
                                    e.config.includeActors

                                _ ->
                                    Nothing
                        )
                    |> Expect.equal [ True ]
        , test "an actor in the answer is listed" <|
            \_ ->
                searchBox
                    |> (\search -> Rs.s_autocomplete (search.autocomplete |> Autocomplete.setQuery "binance" |> Autocomplete.setStatus Autocomplete.Fetching) search)
                    |> Update.Search.update (Msg.Search.BrowserGotSearchResult "binance" apiAnswer)
                    |> Tuple.first
                    |> View.Search.searchWithMoreCss Env.viewConfig View.Search.default
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Binance" ]
        , test "the main search box lists at most five actors" <|
            \_ ->
                actorsListed searchBox
                    |> Expect.equal 5
        , test "the actor picker lists all actors the API sends" <|
            \_ ->
                actorsListed (Init.Search.init Model.Search.SearchActorsOnly)
                    |> Expect.equal 8
        , test "a picked actor is kept in the recent searches" <|
            \_ ->
                [ Model.Search.Actor ( "binance", "Binance" ), Model.Search.Label "binance" ]
                    |> Model.Search.filteredRecents (Init.Search.initSearchAddressAndTxs Nothing)
                    |> Expect.equal [ Model.Search.Actor ( "binance", "Binance" ) ]
        ]
