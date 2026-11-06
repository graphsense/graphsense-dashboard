module Util.TagSummaryLabelPreviewTest exposing (suite)

{-| The address tooltip shows one label and a count of the others, instead of
every label joined and cut at 20 characters. The data is a trimmed copy of
the USDC contract's tag summary (0xa0b8…eb48 on eth): one curated actor label
buried under scraped mentions, some blank or differing only in spacing.
-}

import Api.Data
import Dict
import Expect
import Json.Decode
import Test exposing (Test, describe, test)
import Util.Pathfinder.TagSummary as TagSummary


label : String -> Float -> ( String, Api.Data.LabelSummary )
label =
    labelWith [] 0.2


labelWith : List String -> Float -> String -> Float -> ( String, Api.Data.LabelSummary )
labelWith concepts confidence lbl relevance =
    ( String.toLower lbl
    , { actors = Nothing
      , concepts = concepts
      , confidence = confidence
      , count = 1
      , creators = []
      , inheritedFrom = Nothing
      , label = lbl
      , lastmod = 0
      , relevance = relevance
      , sources = []
      , tagTypes = Nothing
      }
    )


tagged : Maybe (List String) -> Maybe (List String) -> String -> Float -> Float -> ( String, Api.Data.LabelSummary )
tagged actors tagTypes lbl confidence relevance =
    labelWith [] confidence lbl relevance
        |> Tuple.mapSecond (\ls -> { ls | actors = actors, tagTypes = tagTypes })


inherited : Api.Data.LabelSummaryInheritedFrom -> ( String, Api.Data.LabelSummary ) -> ( String, Api.Data.LabelSummary )
inherited from =
    Tuple.mapSecond (\ls -> { ls | inheritedFrom = Just from })


{-| The two optional fields of a label the API sends with `extra` added.
-}
decodedFields : String -> Result Json.Decode.Error ( Maybe (List String), Maybe (List String) )
decodedFields extra =
    """{"concepts":[],"confidence":0.5,"count":1,"creators":[],"label":"l","lastmod":0,"relevance":1,"sources":[]"""
        ++ extra
        ++ "}"
        |> Json.Decode.decodeString (Json.Decode.map (\ls -> ( ls.actors, ls.tagTypes )) Api.Data.labelSummaryDecoder)


summary : Maybe String -> Maybe String -> List ( String, Api.Data.LabelSummary ) -> Api.Data.TagSummary
summary bestActor bestLabel labels =
    { bestActor = bestActor
    , bestLabel = bestLabel
    , broadCategory = "entity"
    , conceptTagCloud = Dict.empty
    , labelSummary = Dict.fromList labels
    , tagCount = List.length labels
    , tagCountIndirect = Nothing
    }


usdcLabels : List ( String, Api.Data.LabelSummary )
usdcLabels =
    [ label "dark web" 0.079
    , label " " 0.033
    , label "Circle: USDC" 0.029
    , label "USDC Token" 0.15
    , label "usdc usdc " 0.0047
    , label " usdc usdc " 0.0046
    , label "usdt to usdc exchange swap convert usdt erc20 " 0.011
    ]


suite : Test
suite =
    describe "Util.Pathfinder.TagSummary.labelPreview"
        [ test "an actor's best label leads, even when a mention has more relevance" <|
            \_ ->
                summary (Just "circle") (Just "Circle: USDC") usdcLabels
                    |> TagSummary.labelPreview 1 Nothing
                    |> Expect.equal (Just { labels = [ "Circle: USDC" ], more = 4 })
        , test "without an actor the most relevant label leads" <|
            \_ ->
                summary Nothing (Just "Circle: USDC") usdcLabels
                    |> TagSummary.labelPreview 1 Nothing
                    |> Maybe.map .labels
                    |> Expect.equal (Just [ "USDC Token" ])
        , test "blank labels and labels differing only in case or spacing are not counted" <|
            \_ ->
                summary Nothing
                    Nothing
                    [ label " " 0.5
                    , label "usdc usdc " 0.2
                    , label " USDC  usdc " 0.1
                    ]
                    |> TagSummary.labelPreview 1 Nothing
                    |> Expect.equal (Just { labels = [ "usdc usdc" ], more = 0 })
        , test "an actor without a usable best label falls back to relevance" <|
            \_ ->
                summary (Just "circle") (Just "  ") usdcLabels
                    |> TagSummary.labelPreview 1 Nothing
                    |> Maybe.map .labels
                    |> Expect.equal (Just [ "USDC Token" ])
        , test "only blank labels give no preview, so the row is left out" <|
            \_ ->
                summary Nothing Nothing [ label " " 0.5 ]
                    |> TagSummary.labelPreview 1 Nothing
                    |> Expect.equal Nothing
        , test "a concept's tooltip ranks its labels by confidence, three at most" <|
            -- the labels the USDC `nameservice` chip showed, alphabetically
            -- and all in one cell, before this was fixed
            \_ ->
                summary (Just "circle")
                    (Just "USDC Token")
                    [ labelWith [ "nameservice" ] 0.5 "andrekidssaving.deployd.eth" 0.027
                    , labelWith [ "nameservice" ] 0.5 "Circle: USDC Token (circle)" 0.0303
                    , labelWith [ "nameservice" ] 0.5 "eth.verifyvault.eth" 0.0265
                    , labelWith [ "nameservice" ] 0.2 "Owner: langkawimalaysia.eth" 0.0043
                    , labelWith [ "nameservice" ] 0.2 "Owner: mauritiusisland.eth" 0.0042
                    , labelWith [ "nameservice" ] 0.5 "Token: USDC (USDC) [circle]" 0.0302
                    , labelWith [ "defi_token" ] 0.75 "USDC Token" 0.15
                    ]
                    |> TagSummary.labelPreview 3 (Just "nameservice")
                    |> Expect.equal
                        (Just
                            { labels =
                                [ "Circle: USDC Token (circle)"
                                , "Token: USDC (USDC) [circle]"
                                , "andrekidssaving.deployd.eth"
                                ]
                            , more = 3
                            }
                        )
        , test "the actor's label leads a concept's tooltip when it carries the concept" <|
            \_ ->
                summary (Just "circle")
                    (Just "Circle: USDC")
                    [ labelWith [ "defi_token" ] 0.5 "eth.verifyvault.eth" 0.9
                    , labelWith [ "defi_token" ] 0.2 "Circle: USDC" 0.1
                    ]
                    |> TagSummary.labelPreview 3 (Just "defi_token")
                    |> Maybe.map .labels
                    |> Expect.equal (Just [ "Circle: USDC", "eth.verifyvault.eth" ])
        , test "entries keep their labelSummary key, so a chip's tooltip finds its entry" <|
            \_ ->
                summary (Just "circle") (Just "Circle: USDC") usdcLabels
                    |> TagSummary.labelEntries 3 Nothing
                    |> .entries
                    |> Expect.equal
                        [ ( "circle: usdc", "Circle: USDC" )
                        , ( "usdc token", "USDC Token" )
                        , ( "dark web", "dark web" )
                        ]
        , test "actor labels win, and the others are only counted" <|
            \_ ->
                summary (Just "circle")
                    (Just "USDC Token")
                    [ tagged (Just [ "circle" ]) (Just [ "actor" ]) "USDC Token" 0.5 0.15
                    , tagged (Just [ "circle" ]) (Just [ "actor" ]) "Circle: USDC" 0.5 0.03
                    , tagged Nothing (Just [ "actor" ]) "usdc.tkn.eth" 0.9 0.5
                    , tagged Nothing (Just [ "mention" ]) "dark web" 0.9 0.5
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "USDC Token", "Circle: USDC" ], more = 2 })
        , test "without actor labels, other tags win over mentions" <|
            \_ ->
                summary Nothing
                    Nothing
                    [ tagged (Just []) (Just [ "mention" ]) "dark web" 0.9 0.9
                    , tagged (Just []) (Just [ "attribute" ]) "Fiat-backed stablecoin" 0.5 0.02
                    , tagged (Just []) (Just [ "actor", "mention" ]) "usdc.eth" 0.2 0.01
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "Fiat-backed stablecoin", "usdc.eth" ], more = 1 })
        , test "mentions are shown when there is nothing else" <|
            \_ ->
                summary Nothing
                    Nothing
                    [ tagged (Just []) (Just [ "mention" ]) "dark web" 0.2 0.9
                    , tagged (Just []) (Just [ "mention" ]) "telegram mention" 0.2 0.1
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "dark web", "telegram mention" ], more = 0 })
        , test "a concept is ranked among its own labels" <|
            \_ ->
                summary (Just "circle")
                    (Just "USDC Token")
                    [ tagged (Just [ "circle" ]) (Just [ "actor" ]) "USDC Token" 0.5 0.15
                    , labelWith [ "drugs" ] 0.2 "dark web" 0.9
                    ]
                    |> TagSummary.labelPreview 3 (Just "drugs")
                    |> Expect.equal (Just { labels = [ "dark web" ], more = 0 })
        , test "an actor label from the cluster does not hide the address's own actor labels" <|
            \_ ->
                summary (Just "binance")
                    (Just "Binance")
                    [ tagged (Just [ "binance" ]) (Just [ "actor" ]) "Binance" 0.9 0.9
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    , tagged (Just [ "circle" ]) (Just [ "actor" ]) "Circle: USDC" 0.2 0.1
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "Circle: USDC" ], more = 1 })
        , test "an actor label from the cluster does not lead the address's other labels" <|
            -- bestLabel would pull it to the front of its tier
            \_ ->
                summary (Just "binance")
                    (Just "Binance")
                    [ tagged (Just [ "binance" ]) (Just [ "actor" ]) "Binance" 0.2 0.1
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    , tagged Nothing (Just [ "attribute" ]) "Hot wallet" 0.9 0.9
                    ]
                    |> TagSummary.labelPreview 1 Nothing
                    |> Expect.equal (Just { labels = [ "Hot wallet" ], more = 1 })
        , test "an actor label from the cluster is shown when the address has nothing else" <|
            \_ ->
                summary (Just "binance")
                    (Just "Binance")
                    [ tagged (Just [ "binance" ]) (Just [ "actor" ]) "Binance" 0.9 0.9
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    , tagged (Just []) (Just [ "mention" ]) "dark web" 0.9 0.9
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "Binance" ], more = 1 })
        , test "with only labels from the cluster, its actor labels win and bestLabel leads" <|
            \_ ->
                summary (Just "binance")
                    (Just "Binance")
                    [ tagged (Just [ "binance" ]) (Just [ "actor" ]) "Binance 14" 0.9 0.9
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    , tagged (Just [ "binance" ]) (Just [ "actor" ]) "Binance" 0.2 0.1
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    , tagged Nothing (Just [ "attribute" ]) "Hot wallet" 0.9 0.9
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "Binance", "Binance 14" ], more = 1 })
        , test "a blank label is not the address's own label" <|
            \_ ->
                summary (Just "binance")
                    (Just "Binance")
                    [ tagged (Just [ "binance" ]) (Just [ "actor" ]) "Binance" 0.2 0.1
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    , tagged Nothing (Just [ "attribute" ]) "Hot wallet" 0.9 0.9
                        |> inherited Api.Data.LabelSummaryInheritedFromCluster
                    , tagged Nothing (Just [ "attribute" ]) " " 0.9 0.9
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "Binance" ], more = 1 })
        , test "an actor label from a shared pubkey stays an actor label" <|
            -- the same key is the same owner; a cluster is only a heuristic
            \_ ->
                summary (Just "circle")
                    (Just "Circle")
                    [ tagged (Just [ "circle" ]) (Just [ "actor" ]) "Circle" 0.5 0.5
                        |> inherited Api.Data.LabelSummaryInheritedFromPubkey
                    , tagged (Just [ "tether" ]) (Just [ "actor" ]) "Tether" 0.4 0.4
                        |> inherited Api.Data.LabelSummaryInheritedFromPubkeyAndCluster
                    , tagged Nothing (Just [ "attribute" ]) "Hot wallet" 0.9 0.9
                    ]
                    |> TagSummary.labelPreview 3 Nothing
                    |> Expect.equal (Just { labels = [ "Circle", "Tether" ], more = 1 })
        , test "the fields are optional in the API" <|
            \_ ->
                [ decodedFields ""
                , decodedFields ""","actors":["circle"],"tag_types":["actor"]"""
                ]
                    |> Expect.equal
                        [ Ok ( Nothing, Nothing )
                        , Ok ( Just [ "circle" ], Just [ "actor" ] )
                        ]
        ]
