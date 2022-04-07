module Main exposing (exampleProgramTest, start)

import ProgramTest exposing (ProgramTest, clickButton, expectViewHas)
import Test exposing (..)
import Test.Html.Selector exposing (class, text)
import Update exposing (update)


start : String -> Flags -> ProgramTest Model Msg (Cmd Msg)
start initialUrl flags =
    ProgramTest.createApplication
        { onUrlChange = MyProgram.OnRouteChange
        , init =
            -- NOTE: the type of MyProgram.init is:
            -- MyProgram.Flags -> Navigation.Location -> (MyProgram.Model, Cmd MyProgram.Msg)
            MyProgram.init
        , update = MyProgram.update
        , view = MyProgram.view
        }
        |> ProgramTest.withBaseUrl initialUrl
        |> ProgramTest.start flags


exampleProgramTest : Test
exampleProgramTest =
    test "pages show social media link at the end" <|
        \() ->
            start "https://app" MyProgram.defaultFlags
                |> expectViewHas
                    [ class "super-social-link"
                    , attribute (href "https://super.social.example.com/avh4")
                    ]
