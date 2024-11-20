module Util.Pathfinder.TagSummary exposing (exchangeCategory, getLabelPreview, hasOnlyExchangeTags, isExchangeNode)

import Api.Data
import Dict
import Util.View


exchangeCategory : String
exchangeCategory =
    "exchange"


hasOnlyExchangeTags : Api.Data.TagSummary -> Bool
hasOnlyExchangeTags tagdata =
    Dict.member exchangeCategory tagdata.conceptTagCloud && Dict.size tagdata.conceptTagCloud == 1


isExchangeNode : Api.Data.TagSummary -> Bool
isExchangeNode tagdata =
    tagdata.broadCategory == exchangeCategory


getLabelPreview : Int -> Api.Data.TagSummary -> String
getLabelPreview n td =
    td.labelSummary
        |> Dict.values
        |> List.sortBy .relevance
        |> List.reverse
        |> List.map .label
        |> String.join ", "
        |> Util.View.truncate n
