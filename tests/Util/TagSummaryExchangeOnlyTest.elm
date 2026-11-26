module Util.TagSummaryExchangeOnlyTest exposing (suite)

{-| Whether an address carries exchange tags only, which decides between the
exchange icon and the tag icon on the graph node, in the address panel and in
a tx's in/output list.

An exchange's own deposit, hot or cold wallet tag adds a wallet-role concept
next to `exchange`. Counted by concept, such an address had "other tags" and
gained the tag icon once its summary loaded (ticked in the output list of btc
tx c239ae18…), while the output list had shown the exchange icon from its
one-tag preview. The data is 1N9m549uY4szkVkR54cVp8JeHc4AqZpHdz on btc
(2026-11-24): "Poloniex.com" and "Poloniex Deposit", both category exchange.

-}

import Api.Data
import Dict
import Expect
import Test exposing (Test, describe, test)
import Util.Pathfinder.TagSummary as TagSummary


label : String -> List String -> ( String, Api.Data.LabelSummary )
label lbl concepts =
    ( String.toLower lbl
    , { actors = Nothing
      , concepts = concepts
      , confidence = 1
      , count = 1
      , creators = []
      , inheritedFrom = Nothing
      , label = lbl
      , lastmod = 0
      , relevance = 1
      , sources = []
      , tagTypes = Nothing
      }
    )


summary : List String -> List ( String, Api.Data.LabelSummary ) -> Api.Data.TagSummary
summary cloud labels =
    { bestActor = Nothing
    , bestLabel = Nothing
    , broadCategory = "exchange"
    , conceptTagCloud = cloud |> List.map (\c -> ( c, { cnt = 1, weighted = 1 } )) |> Dict.fromList
    , labelSummary = Dict.fromList labels
    , tagCount = List.length labels
    , tagCountIndirect = Nothing
    }


suite : Test
suite =
    describe "Util.Pathfinder.TagSummary.hasOnlyExchangeTags"
        [ test "an exchange tag next to its own deposit wallet tag is exchange only" <|
            \_ ->
                summary [ "exchange", "deposit_wallet" ]
                    [ label "Poloniex.com" [ "exchange" ]
                    , label "Poloniex Deposit" [ "deposit_wallet", "exchange" ]
                    ]
                    |> TagSummary.hasOnlyExchangeTags
                    |> Expect.equal True
        , test "a single exchange tag is exchange only" <|
            \_ ->
                summary [ "exchange" ] [ label "Poloniex.com" [ "exchange" ] ]
                    |> TagSummary.hasOnlyExchangeTags
                    |> Expect.equal True
        , test "an exchange tag next to a tag of another category is not" <|
            \_ ->
                summary [ "exchange", "scam" ]
                    [ label "Poloniex.com" [ "exchange" ]
                    , label "Some scam" [ "scam" ]
                    ]
                    |> TagSummary.hasOnlyExchangeTags
                    |> Expect.equal False
        , test "a wallet tag that names no exchange is not" <|
            \_ ->
                summary [ "exchange", "cold_wallet" ]
                    [ label "Poloniex.com" [ "exchange" ]
                    , label "Some cold wallet" [ "cold_wallet" ]
                    ]
                    |> TagSummary.hasOnlyExchangeTags
                    |> Expect.equal False
        , test "without labels the concept cloud decides, as before" <|
            \_ ->
                ( summary [ "exchange" ] [] |> TagSummary.hasOnlyExchangeTags
                , summary [ "exchange", "deposit_wallet" ] [] |> TagSummary.hasOnlyExchangeTags
                )
                    |> Expect.equal ( True, False )
        ]
