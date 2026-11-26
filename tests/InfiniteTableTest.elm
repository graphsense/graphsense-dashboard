module InfiniteTableTest exposing (suite)

import Components.InfiniteTable as InfiniteTable
import Expect
import Html.Styled
import Json.Encode
import Table
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


config : InfiniteTable.Config () ()
config =
    InfiniteTable.config
        |> InfiniteTable.withFetch (\_ _ _ -> ())


tableConfig : InfiniteTable.TableConfig String InfiniteTable.Msg
tableConfig =
    { toId = identity
    , columns = [ Table.stringColumn "col" identity ]
    , customizations = Table.defaultCustomizations
    , tag = identity
    , loadingPlaceholderAbove = [ Html.Styled.text "SPINNER_ABOVE" ]
    , loadingPlaceholderBelow = [ Html.Styled.text "SPINNER_BELOW" ]
    }


noFilter : { search : String -> a -> Bool, filter : a -> Bool }
noFilter =
    { search = \_ _ -> True, filter = always True }


{-| A page of `n` rows; rows are 30 px high until measured.
-}
page : Int -> Int -> List String
page from n =
    List.range from (from + n - 1) |> List.map String.fromInt


{-| The table's own scroll event, at `scrollTop` with `scrollHeight` content
in a 300 px high container.
-}
scrolledTo : Float -> Float -> InfiniteTable.Model () String -> Maybe InfiniteTable.Msg
scrolledTo scrollTop scrollHeight model =
    InfiniteTable.view tableConfig [] model
        |> Html.Styled.toUnstyled
        -- the table's own element is the root
        |> Query.fromHtml
        |> Event.simulate
            (Event.custom "scroll"
                (Json.Encode.object
                    [ ( "target"
                      , Json.Encode.object
                            [ ( "scrollTop", Json.Encode.float scrollTop )
                            , ( "scrollHeight", Json.Encode.float scrollHeight )
                            , ( "offsetHeight", Json.Encode.float 300 )
                            , ( "clientHeight", Json.Encode.float 300 )
                            ]
                      )
                    ]
                )
            )
        |> Event.toResult
        |> Result.toMaybe


suite : Test
suite =
    describe "InfiniteTable loading placeholder"
        [ test "is loading after loadFirstPage" <|
            \_ ->
                InfiniteTable.init "test-table" 10
                    |> InfiniteTable.loadFirstPage config
                    |> Tuple.first
                    |> InfiniteTable.isLoading
                    |> Expect.equal True
        , test "shows loading placeholder while first page is loading" <|
            \_ ->
                let
                    ( model, _ ) =
                        InfiniteTable.init "test-table" 10
                            |> InfiniteTable.loadFirstPage config
                in
                InfiniteTable.view tableConfig [] model
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "SPINNER_BELOW" ]
        , test "a page that arrives after the user stopped at the bottom fetches no further page" <|
            \_ ->
                let
                    ( first, _ ) =
                        InfiniteTable.init "test-table" 50
                            |> InfiniteTable.loadFirstPage config

                    -- 50 rows of 30 px: 1500 px of content
                    ( loaded, _, _ ) =
                        InfiniteTable.appendData config noFilter (Just ()) (page 0 50) first

                    -- the user scrolls to the bottom: the next page is fetched
                    ( scrolled, _, fetchedOnScroll ) =
                        scrolledTo 1200 1500 loaded
                            |> Maybe.map (\msg -> InfiniteTable.update config msg loaded)
                            |> Maybe.withDefault ( loaded, Cmd.none, [] )

                    -- and stops; the page arrives and the content is 3000 px,
                    -- far below where the user is
                    ( _, _, fetchedOnArrival ) =
                        InfiniteTable.appendData config noFilter (Just ()) (page 50 50) scrolled
                in
                ( List.length fetchedOnScroll, List.length fetchedOnArrival )
                    |> Expect.equal ( 1, 0 )
        ]
