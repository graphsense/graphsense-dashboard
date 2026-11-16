module Config.View exposing (CharacterDimension, Config, characterDimensionsDecoder, getConceptName, networkServed, setSystemDark, setThemeMode, toCurrency)

import Api.Data
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Model.Currency exposing (Currency(..))
import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale
import Model.ThemeMode as ThemeMode exposing (ThemeMode)


type alias Config =
    { locale : Locale.Model
    , lightmode : Bool -- resolved from themeMode and systemDark; set both through setThemeMode / setSystemDark
    , themeMode : ThemeMode
    , systemDark : Bool -- the browser's prefers-color-scheme: dark, kept live by a port
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
    , liteNetworks : Bool -- off: every request carries X-Ikn-Currency-Opt-Out: all-light, so the gateway drops the lite roles, the API serves the core networks only and nothing reaches the lite data service
    , liteUnavailable : Bool -- runtime state, never saved: the lite blockchains did not answer (the API named the lite service in X-Ikn-Backends-Declined, or a lite request got its 503); the next successful lite answer clears it
    }


setThemeMode : ThemeMode -> Config -> Config
setThemeMode mode config =
    { config | themeMode = mode, lightmode = ThemeMode.isLight config.systemDark mode }


setSystemDark : Bool -> Config -> Config
setSystemDark systemDark config =
    { config | systemDark = systemDark, lightmode = ThemeMode.isLight systemDark config.themeMode }



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
