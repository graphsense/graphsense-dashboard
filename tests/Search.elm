module Search exposing (..)

import Api
import Api.Data
import Effect exposing (Effect(..))
import Expect exposing (Expectation)
import Mockup.Search
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import ProgramTest exposing (..)
import Setup
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Util exposing (ensureAndSimulateHttp)


type alias Program =
    ProgramTest (Model ()) Msg Effect


base : String -> Program
base locale =
    Setup.start "/" { locale = locale }


searchTest : Test
searchTest =
    describe
        "search"
        [ test "search address" <|
            \() ->
                base "de"
                    |> within (Query.find [ id "header" ])
                        (fillInTextarea "a"
                            >> ensureViewHasNot [ id "search-result", containing [ text "abcdefg" ] ]
                            >> fillInTextarea "ab"
                            >> ensureViewHasNot [ id "search-result", containing [ text "abcdefg" ] ]
                            >> fillInTextarea "abc"
                            >> ensureViewHasNot [ id "search-result", containing [ text "abcdefg" ] ]
                            >> fillInTextarea "abcd"
                        )
                    |> ensureAndSimulateHttp "GET"
                        (Api.baseUrl ++ "/search?q=abcd&limit=10")
                        Mockup.Search.abcd
                        Api.Data.encodeSearchResult
                    |> expectViewHas
                        [ id "search-result"
                        , containing
                            [ text "abcdefg"
                            , text "abcdxyz"
                            ]
                        ]
        ]
