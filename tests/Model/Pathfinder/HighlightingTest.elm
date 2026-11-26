module Model.Pathfinder.HighlightingTest exposing (suite)

import Expect
import Model.Pathfinder.GraphFilter as GraphFilter
import Model.Pathfinder.Highlighting as Highlighting exposing (Highlighting)
import Model.Pathfinder.SearchBox exposing (Highlight(..))
import Set
import Support.GraphFilterFixture exposing (addr)
import Test exposing (Test, describe, test)


{-| "a" is kept although the filters would remove it, "b" is hidden.
-}
filtered : Bool -> Highlighting
filtered showHidden =
    let
        init =
            Highlighting.init

        result =
            GraphFilter.emptyResult
    in
    { init
        | filter = { result | greyedAddresses = Set.singleton (addr "a"), hiddenAddresses = Set.singleton (addr "b") }
        , showHidden = showHidden
    }


suite : Test
suite =
    describe "Model.Pathfinder.Highlighting"
        [ test "a kept address is drawn normally while grey mode is off" <|
            \_ -> Highlighting.forNode (filtered False) (addr "a") |> Expect.equal (Just NoHighlight)
        , test "a kept address is dimmed in grey mode" <|
            \_ -> Highlighting.forNode (filtered True) (addr "a") |> Expect.equal (Just Dimmed)
        , test "a hidden address is not drawn while grey mode is off" <|
            \_ -> Highlighting.forNode (filtered False) (addr "b") |> Expect.equal Nothing
        , test "a hidden address is dimmed in grey mode" <|
            \_ -> Highlighting.forNode (filtered True) (addr "b") |> Expect.equal (Just Dimmed)
        , test "an address the filters do not touch is drawn normally" <|
            \_ -> Highlighting.forNode (filtered False) (addr "c") |> Expect.equal (Just NoHighlight)
        ]
