module Util.TagSummaryLabelPreviewTest exposing (suite)

{-| The address tooltip shows one label and a count of the others, instead of
every label joined and cut at 20 characters. The data is a trimmed copy of
the USDC contract's tag summary (0xa0b8…eb48 on eth): one curated actor label
buried under scraped mentions, some blank or differing only in spacing.
-}

import Api.Data
import Dict
import Expect
import Test exposing (Test, describe, test)
import Util.Pathfinder.TagSummary as TagSummary


label : String -> Float -> ( String, Api.Data.LabelSummary )
label =
    labelWith [] 0.2


labelWith : List String -> Float -> String -> Float -> ( String, Api.Data.LabelSummary )
labelWith concepts confidence lbl relevance =
    ( String.toLower lbl
    , { concepts = concepts
      , confidence = confidence
      , count = 1
      , creators = []
      , inheritedFrom = Nothing
      , label = lbl
      , lastmod = 0
      , relevance = relevance
      , sources = []
      }
    )


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
        , test "ranked labels keep their labelSummary key, so a chip's tooltip finds its entry" <|
            \_ ->
                summary (Just "circle") (Just "Circle: USDC") usdcLabels
                    |> TagSummary.rankedLabels Nothing
                    |> List.take 3
                    |> Expect.equal
                        [ ( "circle: usdc", "Circle: USDC" )
                        , ( "usdc token", "USDC Token" )
                        , ( "dark web", "dark web" )
                        ]
        ]
