module Config.View exposing (Config, getAbuseName, getConceptName, toCurrency)

import Api.Data
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
    }


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
