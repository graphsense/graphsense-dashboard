module Util.Pathfinder.ListTag exposing (ListTag(..), fromTagSummary)

{-| Black and white lists get their own tag icon colour: black for a blacklist,
white for a whitelist. They are recognised by the `black_list` and `white_list`
concepts of the tagpack taxonomy and their governmental variants
`gov_black_list` and `gov_white_list`, which look the same.
-}

import Api.Data
import Basics.Extra exposing (flip)
import Dict


type ListTag
    = Blacklist
    | Whitelist


blacklistConcepts : List String
blacklistConcepts =
    [ "black_list", "gov_black_list" ]


whitelistConcepts : List String
whitelistConcepts =
    [ "white_list", "gov_white_list" ]


{-| Only the address's own tags count. A summary fetched with the best cluster
tag also carries labels inherited from the cluster or a shared pubkey, and a
listing of some other address must not colour this one, so those labels are
skipped. `conceptTagCloud` cannot be used: it does not say where a concept
came from.

A blacklist wins over a whitelist: an address on both is shown as listed.

-}
fromTagSummary : Api.Data.TagSummary -> Maybe ListTag
fromTagSummary tagdata =
    let
        directConcepts =
            tagdata.labelSummary
                |> Dict.values
                |> List.filter (.inheritedFrom >> (==) Nothing)
                |> List.concatMap .concepts
    in
    if List.any (flip List.member directConcepts) blacklistConcepts then
        Just Blacklist

    else if List.any (flip List.member directConcepts) whitelistConcepts then
        Just Whitelist

    else
        Nothing
