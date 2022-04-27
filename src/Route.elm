module Route exposing (Route(..), toUrl)

import Url exposing (..)
import Url.Builder exposing (..)


type Route
    = Address { currency : String, address : String }
    | Block { currency : String, block : Int }
    | Tx { currency : String, tx : String }
    | Label String


addressSegment : String
addressSegment =
    "address"


blockSegment : String
blockSegment =
    "block"


txSegment : String
txSegment =
    "tx"


labelSegment : String
labelSegment =
    "label"


toUrl : Route -> String
toUrl route =
    case route of
        Address { currency, address } ->
            absolute [ currency, addressSegment, address ] []

        Block { currency, block } ->
            absolute [ currency, blockSegment, String.fromInt block ] []

        Tx { currency, tx } ->
            absolute [ currency, txSegment, tx ] []

        Label l ->
            absolute [ labelSegment, l ] []
