module Route exposing (Route(..), Thing(..), parse, toUrl)

import List.Extra
import Url exposing (..)
import Url.Builder as B exposing (..)
import Url.Parser as P exposing (..)


type alias Config =
    { currencies : List String
    }


type Route
    = Currency String Thing
    | Label String


type Thing
    = Address String
    | Block Int
    | Tx String


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
        Currency curr (Address address) ->
            absolute [ curr, addressSegment, address ] []

        Currency curr (Block block) ->
            absolute [ curr, blockSegment, String.fromInt block ] []

        Currency curr (Tx tx) ->
            absolute [ curr, txSegment, tx ] []

        Label l ->
            absolute [ labelSegment, l ] []


parse : Config -> Url -> Maybe Route
parse c =
    P.parse (parser c)


parser : Config -> Parser (Route -> a) a
parser c =
    oneOf
        [ map Currency (currency c </> thing)
        , map Label P.string
        ]


currency : Config -> Parser (String -> a) a
currency c =
    P.custom "CURRENCY" <|
        \segment ->
            List.Extra.find ((==) segment) c.currencies


thing : Parser (Thing -> a) a
thing =
    oneOf
        [ s addressSegment
            </> P.string
            |> map Address
        , s blockSegment
            </> P.int
            |> map Block
        , s txSegment
            </> P.string
            |> map Tx
        ]
