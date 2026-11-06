module Util.Pathfinder.TagSummary exposing (LabelPreview, exchangeCategory, hasOnlyExchangeTags, isExchangeNode, labelEntries, labelPreview)

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


{-| The labels of `labelEntries` without their keys. Nothing when no label is
left to show.
-}
labelPreview : Int -> Maybe String -> Api.Data.TagSummary -> Maybe LabelPreview
labelPreview n concept td =
    let
        { entries, more } =
            labelEntries n concept td
    in
    if List.isEmpty entries then
        Nothing

    else
        Just { labels = List.map Tuple.second entries, more = more }


{-| The address's best labels, at most `n`, each with its key in `labelSummary`
and its cleaned text, and how many other labels there are; with a concept, only
the labels carrying it.

Only the best tier that has labels is shown (see `labelTier`): the other labels
are in the count, however few the shown ones are. Within a tier, the summary's
`bestLabel` leads when it names an actor, the rest follow by confidence, then
relevance.

An actor label inherited from the cluster counts as a plain label as soon as the
address has a label of its own: a cluster is a heuristic and does not outrank
what is known about the address itself. If all labels come from the cluster, it
is the best there is and ranks as usual.

Labels are trimmed and their inner whitespace collapsed; blank ones are
dropped, and labels that differ only in case or spacing count once. Scraped
mentions produce plenty of both (`" "`, `"usdc usdc "` beside `" usdc usdc "`).

-}
labelEntries : Int -> Maybe String -> Api.Data.TagSummary -> { entries : List ( String, String ), more : Int }
labelEntries n concept td =
    let
        candidates =
            td.labelSummary
                |> Dict.toList
                |> List.filter
                    (\( _, ls ) ->
                        concept
                            |> Maybe.map (\c -> List.member c ls.concepts)
                            |> Maybe.withDefault True
                    )

        hasOwnLabel =
            candidates
                |> List.any (\( _, ls ) -> not (isFromCluster ls) && cleanLabel ls.label /= Nothing)

        outranked ls =
            hasOwnLabel && isFromCluster ls

        isActorLabel ls =
            hasActors ls && not (outranked ls)

        isBestLabel lbl =
            td.bestActor
                |> Maybe.andThen (\_ -> td.bestLabel)
                |> Maybe.andThen cleanLabel
                |> Maybe.map (\best -> String.toLower best == String.toLower lbl)
                |> Maybe.withDefault False

        ranked =
            candidates
                |> List.sortWith
                    (\( _, a ) ( _, b ) ->
                        case compare (labelTier (isActorLabel a) a) (labelTier (isActorLabel b) b) of
                            EQ ->
                                case compare b.confidence a.confidence of
                                    EQ ->
                                        compare b.relevance a.relevance

                                    ord ->
                                        ord

                            ord ->
                                ord
                    )
                |> List.filterMap
                    (\( key, ls ) ->
                        cleanLabel ls.label
                            |> Maybe.map
                                (\lbl ->
                                    { tier = labelTier (isActorLabel ls) ls
                                    , canLead = not (outranked ls)
                                    , entry = ( key, lbl )
                                    }
                                )
                    )
                |> List.Extra.uniqueBy (.entry >> Tuple.second >> String.toLower)

        leadingTier =
            ranked |> List.head |> Maybe.map .tier

        ( bestEntries, others ) =
            ranked
                |> List.filter (.tier >> Just >> (==) leadingTier)
                |> List.partition (\{ canLead, entry } -> canLead && isBestLabel (Tuple.second entry))
                |> Tuple.mapBoth (List.map .entry) (List.map .entry)

        entries =
            (bestEntries ++ others) |> List.take n
    in
    { entries = entries
    , more = List.length ranked - List.length entries
    }


hasActors : Api.Data.LabelSummary -> Bool
hasActors ls =
    ls.actors |> Maybe.withDefault [] |> List.isEmpty |> not


{-| Only from the cluster: a shared pubkey is the same owner, not a heuristic.
-}
isFromCluster : Api.Data.LabelSummary -> Bool
isFromCluster ls =
    ls.inheritedFrom == Just Api.Data.LabelSummaryInheritedFromCluster


{-| Which labels come first, lowest first: labels of an actor; labels of other
tags that state something about the address; mentions, which only report where
it was seen.

`actors` and `tagTypes` are optional in the API. Without them every label is in
the middle tier, so nothing is held back on a guess.

-}
labelTier : Bool -> Api.Data.LabelSummary -> Int
labelTier isActorLabel ls =
    if isActorLabel then
        0

    else
        case ls.tagTypes of
            Just ((_ :: _) as types) ->
                if List.all ((==) "mention") types then
                    2

                else
                    1

            _ ->
                1


cleanLabel : String -> Maybe String
cleanLabel lbl =
    -- String.words " " is [ "" ], not []
    case String.words lbl |> List.filter (String.isEmpty >> not) of
        [] ->
            Nothing

        words ->
            Just (String.join " " words)
