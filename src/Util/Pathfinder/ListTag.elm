module Util.Pathfinder.ListTag exposing (ListTag(..), fromTagSummary, tagIconAttr)

{-| Black and white lists get their own tag icon colour: black for a blacklist,
white for a whitelist. They are recognised by the `black_list` and `white_list`
concepts of the tagpack taxonomy and their governmental variants
`gov_black_list` and `gov_white_list`, which look the same.
-}

import Api.Data
import Basics.Extra exposing (flip)
import Css
import Dict
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Theme.Colors as Colors
import Util.View exposing (testId)


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


{-| For the `tagIcon` node of the generated tag icons.

The colours are the light-mode literals on purpose: `--c-black0` and
`--c-white` swap in dark mode, which would turn a blacklist icon white. The
outline in the opposite colour keeps the white icon visible on a light
background and the black one on a dark background.

-}
tagIconAttr : ListTag -> List (Attribute msg)
tagIconAttr list =
    let
        ( fill, outline, name ) =
            case list of
                Blacklist ->
                    ( Colors.black0_string, Colors.white_string, "gs-blacklist-tag" )

                Whitelist ->
                    ( Colors.white_string, Colors.black0_string, "gs-whitelist-tag" )
    in
    [ testId name
    , css
        [ Css.important (Css.property "fill" fill)
        , Css.important (Css.property "stroke" outline)
        , Css.important (Css.property "stroke-width" "1")
        ]
    ]
