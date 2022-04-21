module Stats exposing (statsTest)

import Api
import Effect exposing (Effect(..))
import Expect exposing (Expectation)
import Mockup.Stats
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import ProgramTest exposing (..)
import Setup
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


type alias Program =
    ProgramTest (Model ()) Msg Effect


base : String -> Program
base locale =
    Setup.start "/" { locale = locale }
        |> ProgramTest.ensureHttpRequestWasMade "GET" (Api.baseUrl ++ "/stats")
        |> ProgramTest.simulateHttpOk
            "GET"
            (Api.baseUrl ++ "/stats")
            Mockup.Stats.statsEncoded


statsTest : Test
statsTest =
    describe
        "stats page"
        [ test "stats headings" <|
            \() ->
                base "de"
                    |> ensureViewHas [ tag "h2", text "ledger statistics" ]
                    |> ensureViewHas [ tag "h3", text "Bitcoin" ]
                    |> ensureViewHas [ tag "h3", text "Litecoin" ]
                    |> expectViewHasNot [ tag "h3", text "Ethereum" ]
        , test "english stats data" <|
            \() ->
                base "en"
                    |> ensureViewHas
                        [ containing
                            [ statsRow "Addresses" "100" |> containing
                            , statsRow "Entities" "100" |> containing
                            , statsRow "Labels" "50" |> containing
                            , statsRow "Tagged addresses" "70" |> containing
                            , statsRow "Transactions" "300" |> containing
                            , statsRow "Latest block" "149" |> containing
                            , statsRow "Last update" "01/15/1970 5:40 AM" |> containing
                            ]
                        ]
                    |> expectViewHas
                        [ containing
                            [ statsRow "Addresses" "1,000,000" |> containing
                            , statsRow "Entities" "1,000" |> containing
                            , statsRow "Labels" "500" |> containing
                            , statsRow "Tagged addresses" "700" |> containing
                            , statsRow "Transactions" "3,000" |> containing
                            , statsRow "Latest block" "1,499" |> containing
                            , statsRow "Last update" "01/02/1970 10:10 AM" |> containing
                            ]
                        ]
        , test "german stats data" <|
            \() ->
                base "de"
                    |> ensureViewHas
                        [ containing
                            [ statsRow "Addresses" "100" |> containing
                            , statsRow "Entities" "100" |> containing
                            , statsRow "Labels" "50" |> containing
                            , statsRow "Tagged addresses" "70" |> containing
                            , statsRow "Transactions" "300" |> containing
                            , statsRow "Latest block" "149" |> containing
                            , statsRow "Last update" "15. Januar 1970 05:40" |> containing
                            ]
                        ]
                    |> expectViewHas
                        [ containing
                            [ statsRow "Addresses" "1.000.000" |> containing
                            , statsRow "Entities" "1.000" |> containing
                            , statsRow "Labels" "500" |> containing
                            , statsRow "Tagged addresses" "700" |> containing
                            , statsRow "Transactions" "3.000" |> containing
                            , statsRow "Latest block" "1.499" |> containing
                            , statsRow "Last update" "2. Januar 1970 10:10" |> containing
                            ]
                        ]
        ]


statsRow : String -> String -> List Selector
statsRow key value =
    [ containing [ text key ], containing [ text value ] ]
