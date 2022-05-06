module Model.Locale exposing (..)

import DateFormat.Language
import Dict exposing (Dict)
import FormatNumber.Locales
import Time


locales : List ( String, String )
locales =
    [ ( "de", "german" ), ( "en", "english" ) ]


type State
    = Empty
    | Transition (Dict String String) (Dict String String) Float
    | Settled (Dict String String)


{-|

    locale : the two digit locale id
    hint : a number formatted via JS toLocaleString to derive number formatting
           rules from (see https://package.elm-lang.org/packages/cuducos/elm-format-number/latest/FormatNumber-Locales)

-}
type alias Flags =
    { locale : String
    }


type alias Model =
    { mapping : State
    , numberFormat : FormatNumber.Locales.Locale
    , locale : String
    , zone : Time.Zone
    , timeLang : DateFormat.Language.Language
    , currency : Currency
    }


type Currency
    = Coin
    | Fiat String
