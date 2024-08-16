module Util.Pathfinder.TagSummary exposing (..)

import Api.Data
import Dict


hasOnlyExchangeTags : Api.Data.TagSummary -> Bool
hasOnlyExchangeTags tagdata =
    Dict.member "exchange" tagdata.conceptTagCloud && Dict.size tagdata.conceptTagCloud == 1
