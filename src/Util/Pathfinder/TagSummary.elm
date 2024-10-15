module Util.Pathfinder.TagSummary exposing (exchangeCategory, hasOnlyExchangeTags, isExchangeNode)

import Api.Data
import Dict


exchangeCategory : String
exchangeCategory =
    "exchange"


hasOnlyExchangeTags : Api.Data.TagSummary -> Bool
hasOnlyExchangeTags tagdata =
    Dict.member exchangeCategory tagdata.conceptTagCloud && Dict.size tagdata.conceptTagCloud == 1


isExchangeNode : Api.Data.TagSummary -> Bool
isExchangeNode tagdata =
    tagdata.broadCategory == exchangeCategory
