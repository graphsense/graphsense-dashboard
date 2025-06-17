module Route.Pathfinder exposing (AddressHopType(..), Config, PathHopType(..), Route(..), Thing(..), addressRoute, aggEdgeRoute, parser, pathRoute, toUrl, txRoute)

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
    | Path String (List PathHopType)


type PathHopType
    = AddressHop AddressHopType String
    | TxHop String


type AddressHopType
    = VictimAddress
    | PerpetratorAddress
    | NormalAddress


hopToString : PathHopType -> String
hopToString h =
    case h of
        AddressHop VictimAddress a ->
            "VA_" ++ a

        AddressHop PerpetratorAddress a ->
            "PA_" ++ a

        AddressHop NormalAddress a ->
            "HA_" ++ a

        TxHop t ->
            "T_" ++ t


stringToHop : String -> Maybe PathHopType
stringToHop s =
    if String.startsWith "VA_" s then
        Just (AddressHop VictimAddress (String.dropLeft 3 s))

    else if String.startsWith "PA_" s then
        Just (AddressHop PerpetratorAddress (String.dropLeft 3 s))

    else if String.startsWith "HA_" s then
        Just (AddressHop NormalAddress (String.dropLeft 3 s))

    else if String.startsWith "T_" s then
        Just (TxHop (String.dropLeft 2 s))

    else
        Nothing


type Thing
    = Address String
    | Tx String
    | Block Int
    | Relation String String


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
                , String.join pathSeparator (steps |> List.map hopToString)
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

        Relation a b ->
            ( [ "relation", a, b ], [] )


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


relationSegment : String
relationSegment =
    "relation"


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
        , s relationSegment
            |> P.slash P.string
            |> P.slash P.string
            |> map Relation
        ]


parsePath : Parser (List PathHopType -> a) a
parsePath =
    P.custom "PATH" <|
        \segment ->
            Just (String.split pathSeparator segment |> List.filterMap stringToHop)


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


pathRoute : String -> List PathHopType -> Route
pathRoute network path =
    Path network path


aggEdgeRoute : { network : String, a : String, b : String } -> Route
aggEdgeRoute { network, a, b } =
    Relation a b
        |> Network network
