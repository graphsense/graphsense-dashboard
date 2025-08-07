module Config.View exposing (CharacterDimension, Config, characterDimensionsDecoder, getAbuseName, getConceptName, toCurrency)

import Api.Data
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Model.Currency exposing (Currency(..))
import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale
import Theme.Theme exposing (Theme)


type alias Config =
    { theme : Theme
    , locale : Locale.Model
    , lightmode : Bool
    , size : Maybe BBox -- position and size of the main pane
    , showDatesInUserLocale : Bool
    , showTimeZoneOffset : Bool
    , showTimestampOnTxEdge : Bool
    , preferredFiatCurrency : String
    , showValuesInFiat : Bool
    , showLabelsInTaggingOverview : Bool
    , allConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    , characterDimensions : Dict String { width : Float, height : Float }
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
    List.Extra.find (.id >> (==) cat) vc.allConcepts
        |> Maybe.map .label


getAbuseName : { t | abuseConcepts : List Api.Data.Concept } -> Maybe String -> Maybe String
getAbuseName gc =
    Maybe.andThen (\cat -> List.Extra.find (.id >> (==) cat) gc.abuseConcepts)
        >> Maybe.map .label


toCurrency : Config -> Currency
toCurrency { showValuesInFiat, preferredFiatCurrency } =
    if showValuesInFiat then
        Fiat preferredFiatCurrency

    else
        Coin
