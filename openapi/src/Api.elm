module Api exposing
    ( Request
    , baseUrl
    , effect
    , noExternalTransactions
    , request
    , sendAndAlsoReceiveHeaders
    , withHeader
    , withTracker
    )

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import ProgramTest
import SimulatedEffect.Http
import Url.Builder


type Request a
    = Request
        { method : String
        , headers : List ( String, Maybe String )
        , basePath : String
        , pathParams : List String
        , queryParams : List Url.Builder.QueryParameter
        , body : Maybe Json.Encode.Value
        , decoder : Json.Decode.Decoder a
        , timeout : Maybe Float
        , tracker : Maybe String
        }


baseUrl : String
baseUrl =
    "http://localhost:9000"


noExternalTransactions : String
noExternalTransactions =
    "no external transactions"


request : String -> String -> List ( String, String ) -> List ( String, Maybe String ) -> List ( String, Maybe String ) -> Maybe Json.Encode.Value -> Json.Decode.Decoder a -> Request a
request method path pathParams queryParams headerParams body decoder =
    Request
        { method = method
        , headers = headerParams
        , basePath = baseUrl
        , pathParams = interpolatePath path pathParams
        , queryParams = queries queryParams
        , body = body
        , decoder = decoder
        , timeout = Nothing
        , tracker = Nothing
        }


effect : (Result Http.Error a -> msg) -> Request a -> ProgramTest.SimulatedEffect msg
effect toMsg (Request req) =
    SimulatedEffect.Http.request
        { method = req.method
        , headers = effectHeaders req.headers
        , url = Url.Builder.crossOrigin req.basePath req.pathParams req.queryParams
        , body = Maybe.withDefault SimulatedEffect.Http.emptyBody (Maybe.map SimulatedEffect.Http.jsonBody req.body)
        , expect = effectExpectJson identity toMsg req.decoder
        , timeout = req.timeout
        , tracker = req.tracker
        }


sendAndAlsoReceiveHeaders : (Result ( Http.Error, eff ) ( Dict String String, msg ) -> msg) -> eff -> (a -> msg) -> Request a -> Cmd msg
sendAndAlsoReceiveHeaders wrapMsg eff toMsg (Request req) =
    Http.riskyRequest
        { method = req.method
        , headers = headers req.headers
        , url = Url.Builder.crossOrigin req.basePath req.pathParams req.queryParams
        , body = Maybe.withDefault Http.emptyBody (Maybe.map Http.jsonBody req.body)
        , expect = expectJsonWithHeaders toMsg eff wrapMsg req.decoder
        , timeout = req.timeout
        , tracker = req.tracker
        }


expectJsonWithHeaders : (a -> msg) -> eff -> (Result ( Http.Error, eff ) ( Dict String String, msg ) -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
expectJsonWithHeaders toMsg eff wrapMsg decoder =
    Http.expectStringResponse wrapMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ( Http.BadUrl url, eff )

                Http.Timeout_ ->
                    Err ( Http.Timeout, eff )

                Http.NetworkError_ ->
                    Err ( Http.NetworkError, eff )

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 404 && String.contains "no external transactions" body then
                        Err ( Http.BadBody "no external transactions", eff )

                    else
                        Err ( Http.BadStatus metadata.statusCode, eff )

                Http.GoodStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok ( metadata.headers, toMsg value )

                        Err err ->
                            Err ( Http.BadBody (Json.Decode.errorToString err), eff )


withTracker : String -> Request a -> Request a
withTracker tracker (Request req) =
    Request { req | tracker = Just tracker }


withHeader : String -> String -> Request a -> Request a
withHeader key value (Request req) =
    Request { req | headers = req.headers ++ [ ( key, Just value ) ] }



-- HELPER


headers : List ( String, Maybe String ) -> List Http.Header
headers =
    List.filterMap (\( key, value ) -> Maybe.map (Http.header key) value)


effectHeaders : List ( String, Maybe String ) -> List SimulatedEffect.Http.Header
effectHeaders =
    List.filterMap (\( key, value ) -> Maybe.map (SimulatedEffect.Http.header key) value)


interpolatePath : String -> List ( String, String ) -> List String
interpolatePath rawPath pathParams =
    let
        interpolate =
            \( name, value ) path -> String.replace ("{" ++ name ++ "}") value path
    in
    List.foldl interpolate rawPath pathParams
        |> String.split "/"
        |> List.drop 1


queries : List ( String, Maybe String ) -> List Url.Builder.QueryParameter
queries =
    List.filterMap (\( key, value ) -> Maybe.map (Url.Builder.string key) value)


effectExpectJson : (SimulatedEffect.Http.Error -> e) -> (Result e a -> msg) -> Json.Decode.Decoder a -> SimulatedEffect.Http.Expect msg
effectExpectJson mapError toMsg decoder =
    SimulatedEffect.Http.expectStringResponse toMsg (Result.mapError mapError << decodeResponse decoder)


decodeResponse : Json.Decode.Decoder a -> Http.Response String -> Result Http.Error a
decodeResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            if String.isEmpty body then
                -- we might 'expect' no body if the return type is `()`
                case Json.Decode.decodeString decoder "{}" of
                    Ok value ->
                        Ok value

                    Err _ ->
                        decodeBody decoder body

            else
                decodeBody decoder body


decodeBody : Json.Decode.Decoder a -> String -> Result Http.Error a
decodeBody decoder body =
    case Json.Decode.decodeString decoder body of
        Ok value ->
            Ok value

        Err err ->
            Err (Http.BadBody (Json.Decode.errorToString err))
