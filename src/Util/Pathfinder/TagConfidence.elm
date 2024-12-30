module Util.Pathfinder.TagConfidence exposing (ConfidenceRange(..), getConfidenceRangeFromFloat)


type ConfidenceRange
    = Low
    | Medium
    | High


getConfidenceRangeFromFloat : Float -> ConfidenceRange
getConfidenceRangeFromFloat f =
    if f >= 0.8 then
        High

    else if f >= 0.4 then
        Medium

    else
        Low
