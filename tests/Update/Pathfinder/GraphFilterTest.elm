module Update.Pathfinder.GraphFilterTest exposing (suite)

import Expect
import Model.Pathfinder.GraphFilter as GraphFilter exposing (Model, Period, TimeMode(..))
import Msg.Pathfinder.GraphFilter exposing (Msg(..))
import Support.GraphFilterFixture exposing (day)
import Test exposing (Test, describe, test)
import Time
import Update.Pathfinder.GraphFilter as Update


run : List Msg -> Model
run =
    List.foldl (Update.update Time.utc) GraphFilter.init


{-| Whole days, as the strip and the date fields produce them.
-}
days : Int -> Int -> Period
days from until =
    { from = Time.millisToPosix (day from * 1000)
    , until = Time.millisToPosix (day (until + 1) * 1000 - 1)
    }


expectTime : TimeMode -> Maybe Period -> Model -> Expect.Expectation
expectTime mode period model =
    ( model.timeMode, model.period ) |> Expect.equal ( mode, period )


suite : Test
suite =
    describe "Update.Pathfinder.GraphFilter (time)"
        [ test "a typed day picks that whole day" <|
            \_ -> run [ UserChangedDay "1970-01-11" ] |> expectTime OneDay (Just (days 10 10))
        , test "from and until make a range of whole days" <|
            \_ ->
                run [ UserChoseTimeMode DateRange, UserChangedFrom "1970-01-11", UserChangedUntil "1970-01-21" ]
                    |> expectTime DateRange (Just (days 10 20))
        , test "an until before the from swaps them" <|
            \_ ->
                run [ UserChangedFrom "1970-01-21", UserChangedUntil "1970-01-11" ]
                    |> expectTime DateRange (Just (days 10 20))
        , test "a click on a strip bar picks its day" <|
            \_ ->
                run [ UserPressedStripBucket (days 10 10), UserReleasedStripBucket (days 10 10) ]
                    |> Expect.all
                        [ expectTime OneDay (Just (days 10 10))
                        , .stripDrag >> Expect.equal Nothing
                        ]
        , test "a click on a bar of several days picks that range" <|
            \_ ->
                run [ UserPressedStripBucket (days 10 12), UserReleasedStripBucket (days 10 12) ]
                    |> expectTime DateRange (Just (days 10 12))
        , test "a drag over the strip picks the period between, either way" <|
            \_ ->
                run
                    [ UserPressedStripBucket (days 20 20)
                    , UserEnteredStripBucket (days 15 15)
                    , UserEnteredStripBucket (days 12 12)
                    , UserReleasedStripBucket (days 12 12)
                    ]
                    |> expectTime DateRange (Just (days 12 20))
        , test "moving over the strip without pressing picks nothing" <|
            \_ -> run [ UserEnteredStripBucket (days 12 12) ] |> expectTime AnyTime Nothing
        , test "leaving the strip ends a drag" <|
            \_ ->
                run [ UserPressedStripBucket (days 12 12), UserLeftStrip, UserEnteredStripBucket (days 15 15) ]
                    |> expectTime AnyTime Nothing
        , test "one day keeps the first day of a range" <|
            \_ ->
                run [ UserChangedFrom "1970-01-11", UserChangedUntil "1970-01-21", UserChoseTimeMode OneDay ]
                    |> expectTime OneDay (Just (days 10 10))
        , test "any time drops the period" <|
            \_ ->
                run [ UserChangedDay "1970-01-11", UserChoseTimeMode AnyTime ]
                    |> expectTime AnyTime Nothing
        , test "removing the time chip goes back to any time" <|
            \_ ->
                run [ UserChangedDay "1970-01-11", UserChangedPeriod Nothing ]
                    |> expectTime AnyTime Nothing
        , test "reset goes back to any time" <|
            \_ ->
                run [ UserChangedFrom "1970-01-11", UserChangedUntil "1970-01-21", UserClickedReset ]
                    |> expectTime AnyTime Nothing
        , test "a half-typed date changes nothing" <|
            \_ -> run [ UserChangedDay "1970-01-11", UserChangedDay "1970-0" ] |> expectTime OneDay (Just (days 10 10))
        ]
