module Api exposing
    ( Request
    , baseUrl
    , effect
    , map
    , noExternalTransactions
    , request
    , send
    , sendAndAlsoReceiveHeaders
    , sendWithCustomError
    , task
    , withBasePath
    , withBearerToken
    , withHeader
    , withHeaders
    , withTimeout
    , withTracker
    )

import Dict
import Http
import Json.Decode
import Json.Encode
import ProgramTest
import SimulatedEffect.Http
import Task
import Url.Builder
import Util.Http exposing (Headers)


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


send : (Result Http.Error a -> msg) -> Request a -> Cmd msg
send toMsg req =
    sendWithCustomError identity toMsg req


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


sendWithCustomError : (Http.Error -> e) -> (Result e a -> msg) -> Request a -> Cmd msg
sendWithCustomError mapError toMsg (Request req) =
    Http.request
        { method = req.method
        , headers = headers req.headers
        , url = Url.Builder.crossOrigin req.basePath req.pathParams req.queryParams
        , body = Maybe.withDefault Http.emptyBody (Maybe.map Http.jsonBody req.body)
        , expect = expectJson mapError toMsg req.decoder
        , timeout = req.timeout
        , tracker = req.tracker
        }


sendAndAlsoReceiveHeaders : (Result ( Http.Error, Headers, eff ) ( Headers, msg ) -> msg) -> eff -> (a -> msg) -> Request a -> Cmd msg
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


expectJsonWithHeaders : (a -> msg) -> eff -> (Result ( Http.Error, Headers, eff ) ( Headers, msg ) -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
expectJsonWithHeaders toMsg eff wrapMsg decoder =
    Http.expectStringResponse wrapMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ( Http.BadUrl url, Dict.empty, eff )

                Http.Timeout_ ->
                    Err ( Http.Timeout, Dict.empty, eff )

                Http.NetworkError_ ->
                    Err ( Http.NetworkError, Dict.empty, eff )

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 404 && String.contains "no external transactions" body then
                        Err ( Http.BadBody "no external transactions", metadata.headers, eff )

                    else
                        Err ( Http.BadStatus metadata.statusCode, metadata.headers, eff )

                Http.GoodStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok ( metadata.headers, toMsg value )

                        Err err ->
                            Err ( Http.BadBody (Json.Decode.errorToString err), metadata.headers, eff )


task : Request a -> Task.Task Http.Error a
task (Request req) =
    Http.task
        { method = req.method
        , headers = headers req.headers
        , url = Url.Builder.crossOrigin req.basePath req.pathParams req.queryParams
        , body = Maybe.withDefault Http.emptyBody (Maybe.map Http.jsonBody req.body)
        , resolver = jsonResolver req.decoder
        , timeout = req.timeout
        }


map : (a -> b) -> Request a -> Request b
map fn (Request req) =
    Request
        { method = req.method
        , headers = req.headers
        , basePath = req.basePath
        , pathParams = req.pathParams
        , queryParams = req.queryParams
        , body = req.body
        , decoder = Json.Decode.map fn req.decoder
        , timeout = req.timeout
        , tracker = req.tracker
        }


withBasePath : String -> Request a -> Request a
withBasePath basePath (Request req) =
    Request { req | basePath = basePath }


withTimeout : Float -> Request a -> Request a
withTimeout timeout (Request req) =
    Request { req | timeout = Just timeout }


withTracker : String -> Request a -> Request a
withTracker tracker (Request req) =
    Request { req | tracker = Just tracker }


withBearerToken : String -> Request a -> Request a
withBearerToken token (Request req) =
    Request { req | headers = ( "Authorization", Just ("Bearer " ++ token) ) :: req.headers }


withHeader : String -> String -> Request a -> Request a
withHeader key value (Request req) =
    Request { req | headers = req.headers ++ [ ( key, Just value ) ] }


withHeaders : List ( String, String ) -> Request a -> Request a
withHeaders headers_ (Request req) =
    Request { req | headers = req.headers ++ List.map (Tuple.mapSecond Just) headers_ }



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


expectJson : (Http.Error -> e) -> (Result e a -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
expectJson mapError toMsg decoder =
    Http.expectStringResponse toMsg (Result.mapError mapError << decodeResponse decoder)


effectExpectJson : (SimulatedEffect.Http.Error -> e) -> (Result e a -> msg) -> Json.Decode.Decoder a -> SimulatedEffect.Http.Expect msg
effectExpectJson mapError toMsg decoder =
    SimulatedEffect.Http.expectStringResponse toMsg (Result.mapError mapError << decodeResponse decoder)


jsonResolver : Json.Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver (decodeResponse decoder)


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
