module View.DetailLevelTest exposing (suite)

{-| Labels drop out in steps as the graph is zoomed out, and come back for the
node the user is looking at. Pins the thresholds and what each node draws per
level.
-}

import Config.Pathfinder
import Data.Pathfinder.Address as Address
import Data.Pathfinder.Tx as Tx
import Expect
import Html.Styled
import Model.Pathfinder.Address
import Model.Pathfinder.DetailLevel as DetailLevel exposing (DetailLevel(..))
import Model.Pathfinder.SearchBox exposing (Highlight(..))
import Model.Pathfinder.Tx exposing (TxType(..))
import Msg.Pathfinder exposing (Msg)
import Support.App as App
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Util.Data as Data
import View.Locale as Locale
import View.Pathfinder.Address
import View.Pathfinder.Tx


pathfinderConfig : Config.Pathfinder.Config
pathfinderConfig =
    (App.model App.init).config


renderAddress : DetailLevel -> Bool -> Query.Single Msg
renderAddress level selected =
    View.Pathfinder.Address.view Env.viewConfig pathfinderConfig NoHighlight level { address1 | selected = selected } Nothing
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


address1 : Model.Pathfinder.Address.Address
address1 =
    Address.address1


renderTx : DetailLevel -> Bool -> Query.Single Msg
renderTx level selected =
    View.Pathfinder.Tx.view Env.viewConfig pathfinderConfig NoHighlight level { tx1 | selected = selected } Nothing
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


tx1 : Model.Pathfinder.Tx.Tx
tx1 =
    Tx.tx1


{-| The date the tx node prints at full detail (`showTimestampOnTxEdge` is on
in `Support.Env`).
-}
txDate : String
txDate =
    case tx1.type_ of
        Utxo { raw } ->
            Locale.timestampDateUniform Env.viewConfig.locale (Data.timestampToPosix raw.timestamp)

        Account { raw } ->
            Locale.timestampDateUniform Env.viewConfig.locale (Data.timestampToPosix raw.timestamp)


identifier : String
identifier =
    "a1234567"


suite : Test
suite =
    describe "graph detail level"
        [ describe "from zoom"
            [ test "zoomed in is full" <| \_ -> DetailLevel.fromZoom 1 |> Expect.equal Full
            , test "the relationship-mode threshold still separates full from reduced" <|
                \_ -> ( DetailLevel.fromZoom 2.5, DetailLevel.fromZoom 2.6 ) |> Expect.equal ( Full, Reduced )
            , test "far out is minimal" <| \_ -> DetailLevel.fromZoom 10 |> Expect.equal Minimal
            ]
        , describe "address node"
            [ test "shows its identifier at full and reduced detail" <|
                \_ ->
                    Expect.all
                        [ \_ -> renderAddress Full False |> Query.has [ Selector.text identifier ]
                        , \_ -> renderAddress Reduced False |> Query.has [ Selector.text identifier ]
                        ]
                        ()
            , test "drops the identifier at minimal detail" <|
                \_ -> renderAddress Minimal False |> Query.hasNot [ Selector.text identifier ]
            , test "keeps the identifier at minimal detail while selected" <|
                \_ -> renderAddress Minimal True |> Query.has [ Selector.text identifier ]
            ]
        , describe "tx node"
            [ test "shows its date at full detail" <|
                \_ -> renderTx Full False |> Query.has [ Selector.text txDate ]
            , test "drops the date at reduced detail" <|
                \_ -> renderTx Reduced False |> Query.hasNot [ Selector.text txDate ]
            , test "keeps the date at reduced detail while selected" <|
                \_ -> renderTx Reduced True |> Query.has [ Selector.text txDate ]
            ]
        ]
