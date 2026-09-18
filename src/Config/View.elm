module Config.View exposing (CharacterDimension, Config, characterDimensionsDecoder, getConceptName, networkServed, toCurrency)

import Api.Data
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Model.Currency exposing (Currency(..))
import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale


type alias Config =
    { locale : Locale.Model
    , lightmode : Bool
    , size : Maybe BBox -- position and size of the main pane
    , showDatesInUserLocale : Bool
    , showTimeZoneOffset : Bool
    , showTimestampOnTxEdge : Bool
    , preferredFiatCurrency : String
    , showValuesInFiat : Bool
    , showHash : Bool
    , showLabelsInTaggingOverview : Bool
    , showConversionEdges : Bool
    , allConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    , characterDimensions : Dict String { width : Float, height : Float }
    , showBothValues : Bool
    , isMac : Bool -- shortcut hints read Cmd instead of Ctrl
    , networks : List Api.Data.CurrencyStats -- what the backend serves; empty until the statistics arrive
    , liteNetworks : Bool -- off: every request carries X-External-Backends: off, so the API serves the core networks only and nothing reaches the lite data service
    }



-- Type alias for character dimensions


type alias CharacterDimension =
    { width : Float
    , height : Float
    }



-- Decoder for a single character dimension


characterDimensionDecoder : Decoder CharacterDimension
characterDimensionDecoder =
    Decode.map2 CharacterDimension
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)



-- Decoder for the full dictionary


characterDimensionsDecoder : Decoder (Dict String CharacterDimension)
characterDimensionsDecoder =
    Decode.dict characterDimensionDecoder


getConceptName : { t | allConcepts : List Api.Data.Concept } -> String -> Maybe String
getConceptName vc cat =
    if cat == "" then
        Just "Uncategorized"

    else if cat == "unknown" then
        Just "Uncategorized"

    else
        List.Extra.find (.id >> (==) cat) vc.allConcepts
            |> Maybe.map .label


toCurrency : Config -> Currency
toCurrency { showValuesInFiat, preferredFiatCurrency } =
    if showValuesInFiat then
        Fiat preferredFiatCurrency

    else
        Coin


{-| Does the backend currently serve this network? Driven by the statistics
response, which lists only the networks the API serves right now: with the
lite-networks setting off, the lite networks are missing from it, so their
nodes on the graph are drawn faded. Unknown until the statistics arrive.
-}
networkServed : Config -> String -> Bool
networkServed vc network =
    List.isEmpty vc.networks
        || List.any (\n -> String.toLower n.name == String.toLower network) vc.networks
