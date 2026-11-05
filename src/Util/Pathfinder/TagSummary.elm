module Util.Pathfinder.TagSummary exposing (LabelPreview, exchangeCategory, hasOnlyExchangeTags, isExchangeNode, labelPreview, rankedLabels)

import Api.Data
import Dict
import List.Extra


exchangeCategory : String
exchangeCategory =
    "exchange"


hasOnlyExchangeTags : Api.Data.TagSummary -> Bool
hasOnlyExchangeTags tagdata =
    Dict.member exchangeCategory tagdata.conceptTagCloud && Dict.size tagdata.conceptTagCloud == 1


isExchangeNode : Api.Data.TagSummary -> Bool
isExchangeNode tagdata =
    tagdata.broadCategory == exchangeCategory


{-| The labels to show where only a few fit (the tooltips), and how many
other distinct labels there are.
-}
type alias LabelPreview =
    { labels : List String
    , more : Int
    }


{-| The first `n` of `rankedLabels`, and a count of the rest. Nothing when no
label is left to show.
-}
labelPreview : Int -> Maybe String -> Api.Data.TagSummary -> Maybe LabelPreview
labelPreview n concept td =
    case rankedLabels concept td |> List.map Tuple.second of
        [] ->
            Nothing

        labels ->
            Just
                { labels = List.take n labels
                , more = max 0 (List.length labels - n)
                }


{-| The address's labels, best first, each with its key in `labelSummary` and
its cleaned text; with a concept, only the labels carrying it.

Labels from actor tags win: when the summary names an actor, its `bestLabel`
(which the tag digest picks among that actor's labels) comes first, however
many better-scored mentions there are. The rest follow by confidence, then
relevance, so a curated label is not buried under a pile of scraped mentions
that share its words.

Labels are trimmed and their inner whitespace collapsed; blank ones are
dropped, and labels that differ only in case or spacing count once. Scraped
mentions produce plenty of both (`" "`, `"usdc usdc "` beside `" usdc usdc "`).

-}
rankedLabels : Maybe String -> Api.Data.TagSummary -> List ( String, String )
rankedLabels concept td =
    let
        labels =
            td.labelSummary
                |> Dict.toList
                |> List.filter
                    (\( _, ls ) ->
                        concept
                            |> Maybe.map (\c -> List.member c ls.concepts)
                            |> Maybe.withDefault True
                    )
                |> List.sortWith
                    (\( _, a ) ( _, b ) ->
                        case compare b.confidence a.confidence of
                            EQ ->
                                compare b.relevance a.relevance

                            ord ->
                                ord
                    )
                |> List.filterMap (\( key, ls ) -> cleanLabel ls.label |> Maybe.map (Tuple.pair key))
                |> List.Extra.uniqueBy (Tuple.second >> String.toLower)

        isActorLabel ( _, lbl ) =
            td.bestActor
                |> Maybe.andThen (\_ -> td.bestLabel)
                |> Maybe.andThen cleanLabel
                |> Maybe.map (\best -> String.toLower best == String.toLower lbl)
                |> Maybe.withDefault False

        ( actorLabels, others ) =
            List.partition isActorLabel labels
    in
    actorLabels ++ others


cleanLabel : String -> Maybe String
cleanLabel lbl =
    -- String.words " " is [ "" ], not []
    case String.words lbl |> List.filter (String.isEmpty >> not) of
        [] ->
            Nothing

        words ->
            Just (String.join " " words)
