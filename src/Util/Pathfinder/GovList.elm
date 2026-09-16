module Util.Pathfinder.GovList exposing (GovList(..), fromTagSummary, tagIconAttr)

{-| Governmental black and white lists get their own tag icon colour: black for
a blacklist, white for a whitelist. They are recognised by the `gov_black_list`
and `gov_white_list` concepts of the tagpack taxonomy.
-}

import Api.Data
import Css
import Dict
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Theme.Colors as Colors
import Util.View exposing (testId)


type GovList
    = GovBlackList
    | GovWhiteList


govBlackListConcept : String
govBlackListConcept =
    "gov_black_list"


govWhiteListConcept : String
govWhiteListConcept =
    "gov_white_list"


{-| Only the address's own tags count. A summary fetched with the best cluster
tag also carries labels inherited from the cluster or a shared pubkey, and a
listing of some other address must not colour this one, so those labels are
skipped. `conceptTagCloud` cannot be used: it does not say where a concept
came from.

A blacklist wins over a whitelist: an address on both is shown as listed.

-}
fromTagSummary : Api.Data.TagSummary -> Maybe GovList
fromTagSummary tagdata =
    let
        directConcepts =
            tagdata.labelSummary
                |> Dict.values
                |> List.filter (.inheritedFrom >> (==) Nothing)
                |> List.concatMap .concepts
    in
    if List.member govBlackListConcept directConcepts then
        Just GovBlackList

    else if List.member govWhiteListConcept directConcepts then
        Just GovWhiteList

    else
        Nothing


{-| For the `tagIcon` node of the generated tag icons.

The colours are the light-mode literals on purpose: `--c-black0` and
`--c-white` swap in dark mode, which would turn a blacklist icon white. The
outline in the opposite colour keeps the white icon visible on a light
background and the black one on a dark background.

-}
tagIconAttr : GovList -> List (Attribute msg)
tagIconAttr list =
    let
        ( fill, outline, name ) =
            case list of
                GovBlackList ->
                    ( Colors.black0_string, Colors.white_string, "gs-gov-blacklist-tag" )

                GovWhiteList ->
                    ( Colors.white_string, Colors.black0_string, "gs-gov-whitelist-tag" )
    in
    [ testId name
    , css
        [ Css.important (Css.property "fill" fill)
        , Css.important (Css.property "stroke" outline)
        , Css.important (Css.property "stroke-width" "1")
        ]
    ]
