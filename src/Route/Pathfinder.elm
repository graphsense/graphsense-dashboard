module Route.Pathfinder exposing (Config, Route(..), Thing(..), addressRoute, parser, pathRoute, toUrl, txRoute)

import List.Extra
import Url.Builder exposing (QueryParameter, absolute)
import Util.Url.Parser as P exposing (Parser, map, oneOf, s)


type alias Config =
    { networks : List String
    }


type Route
    = Root
    | Actor String
    | Label String
    | Network String Thing
    | Path String (List String)


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

        Network c si ->
            let
                ( itemPath, itemQuery ) =
                    thingToUrl si
            in
            absolute (c :: itemPath) itemQuery

        Path net steps ->
            absolute
                [ net
                , pathSegment
                , String.join pathSeparator steps
                ]
                []


thingToUrl : Thing -> ( List String, List QueryParameter )
thingToUrl t =
    case t of
        Address a ->
            ( [ "address", a ], [] )

        Tx h ->
            ( [ "tx", h ], [] )

        Block nr ->
            ( [ "block", String.fromInt nr ], [] )


parser : Config -> Parser (Route -> a) a
parser c =
    oneOf
        [ map Network (parseCurrency c |> P.slash thing)
        , map Path (parseCurrency c |> P.slash (P.s pathSegment) |> P.slash parsePath)
        , map Label (P.s "label" |> P.slash P.string)
        , map Actor (P.s "actor" |> P.slash P.string)
        , map Root P.top
        ]


addressSegment : String
addressSegment =
    "address"


blockSegment : String
blockSegment =
    "block"


txSegment : String
txSegment =
    "tx"


pathSegment : String
pathSegment =
    "path"


pathSeparator : String
pathSeparator =
    ","


thing : Parser (Thing -> a) a
thing =
    oneOf
        [ s addressSegment
            |> P.slash P.string
            --|> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToAddressTable))
            |> map Address
        , s txSegment |> P.slash P.string |> map Tx
        , s blockSegment |> P.slash P.int |> map Block
        ]


parsePath : Parser (List String -> a) a
parsePath =
    P.custom "PATH" <|
        \segment ->
            Just (String.split pathSeparator segment)


addressRoute : { network : String, address : String } -> Route
addressRoute { network, address } =
    Address address |> Network network


txRoute : { network : String, txHash : String } -> Route
txRoute { network, txHash } =
    Tx txHash |> Network network


parseCurrency : Config -> Parser (String -> a) a
parseCurrency c =
    P.custom "CURRENCY" <|
        \segment ->
            List.Extra.find ((==) segment) c.networks


pathRoute : String -> List String -> Route
pathRoute network path =
    Path network path
