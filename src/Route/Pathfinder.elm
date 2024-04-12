module Route.Pathfinder exposing (..)

import Url
import Url.Builder as B exposing (..)
import Util.Url.Parser as P exposing (..)
import Util.Url.Parser.Query as Q


type Route
    = Root
    | Currency String Thing
    | Actor String
    | Label String


type Thing
    = Address String
    | Tx String
    | Block Int


toUrl : Route -> String
toUrl r =
    case r of
        Root ->
            absolute [] []

        Actor s ->
            absolute [ "actor", s ] []

        Label s ->
            absolute [ "label", s ] []

        Currency c si ->
            let
                ( itemPath, itemQuery ) =
                    thingToUrl si
            in
            absolute ([ c ] ++ itemPath) ([] ++ itemQuery)


thingToUrl : Thing -> ( List String, List QueryParameter )
thingToUrl t =
    case t of
        Address a ->
            ( [ "address", a ], [] )

        Tx h ->
            ( [ "tx", h ], [] )

        Block nr ->
            ( [ "block", String.fromInt nr ], [] )


parser : Parser (Route -> a) a
parser =
    map Root P.top
