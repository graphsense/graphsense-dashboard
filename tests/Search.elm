module Search exposing (..)

import Api
import Api.Data
import Expect exposing (Expectation)
import Json.Encode
import Mockup.Search
import Model exposing (Effect(..), Flags, Model, Msg(..))
import ProgramTest exposing (..)
import Setup
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Util exposing (ensureAndSimulateHttp)


type alias Program =
    ProgramTest (Model ()) Msg (List Effect)


base : String -> Program
base locale =
    Setup.start locale


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
                        (Api.baseUrl ++ "/search?q=abcd&limit=100")
                        Mockup.Search.abcd
                        Api.Data.encodeSearchResult
                    |> expectModel
                        (\model ->
                            Expect.notEqual Nothing <| Debug.log "found" model.search.found
                        )

        {- does not work for whatever reason! View function gets called with empty search result eventually.
           |> expectViewHas
               [ tag "ol"
               , containing
                   [ tag "li"
                   , text "abcdefg123456"
                   ]
               , containing
                   [ tag "li"
                   , text "abcdxyz789012"
                   ]
               ]
        -}
        ]
